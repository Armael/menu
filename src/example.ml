open Core.Std

(* Auxiliary sources *)
let list_episodes show_name =
  let ic = Unix.open_process_in ("series " ^ show_name) in
  let episodes =
    In_channel.fold_lines ic ~init:[] ~f:(fun acc episode ->
        let (id, name) = String.lsplit2_exn episode ~on:'\t' in
        (name, id, id) :: acc
      )
  in
  ignore (Unix.close_process_in ic) ;
  Source.from_list_rev episodes

let list_shows =
  let ic = Unix.open_process_in "series" in
  let shows =
    In_channel.fold_lines ic ~init:[] ~f:(fun acc show ->
        let show_name, nb_eps = String.lsplit2_exn show ~on:' ' in
        (show_name, show_name, String.lstrip nb_eps) :: acc
      )
  in
  ignore (Unix.close_process_in ic) ;
  Source.from_list_rev shows

let papers =
  let filter absolute_path =
    match Sys.is_file absolute_path with
    | `Yes ->
      begin match snd @@ Filename.split_extension absolute_path with
        | None -> false
        | Some ext -> List.mem ["pdf" ; "ps" ; "djvu" ] ext
      end
    | _ -> false
  in [
    lazy Source.(paths ~coupled_with:(files ~filter "/tmp")) ;
    lazy Source.(paths ~coupled_with:(files ~filter "/home/rks/Papiers")) ;
  ]

(* Completion engines *)
let series = Engine.({
    sources = [ list_shows ] ;
    transition = fun show -> singleton (list_episodes show.Candidate.display)
  })

let engine =
  let open Completion in
  sum Sources.binaries (fun o ->
      let cmd = o#display in
      match cmd with
      | "chromium" -> iterate [ Extra_sources.chromium_bookmarks ]
      | "evince" -> iterate papers
      | "series" -> series
      | _ ->
        sum (Sources.paths ~coupled_with:(Extra_sources.from_file cmd)) (
          fun arg ->
            match cmd, arg#display with
            | "mpc", "load" -> iterate  [ Extra_sources.Mpc.playlists ]
            | "mpc", "play" -> singleton Extra_sources.Mpc.current_playlist
            | _ -> Extra_sources.stm_from_file (cmd ^ arg#display)
        )
    )

(* Main function *)
let run =
  let open Dmlenu in
  let source_transition_hook state conf =
    let open Matching in
    let (match_fun, lines) =
      match List.map state.Completion.entries ~f:(fun (_, _, s) -> s) with
      | "mpc" :: "play" :: _                 -> (subset ~case:false, 20)
      | "mpc" :: _                           -> (fuzzy_match ~case:false, 0)
      | "chromium" :: _ | "evince" :: _      -> (fuzzy_match ~case:false, 20)
      | "series" :: l when List.length l < 2 -> (fuzzy_match ~case:false, 20)
      | _                                    -> (match_prefix ~case:false, 0)
    in
    set_match_query_fun match_fun ;
    { conf with lines }
  in
  let app_state = { prompt = "" ; compl = Completion.make_state engine } in
  let conf = { default_conf with window_background = "#222222" } in
  match run_list ~source_transition_hook app_state conf with
  | [] -> ()
  | prog :: _ as args ->
    never_returns @@ Unix.exec ~prog ~args ()
