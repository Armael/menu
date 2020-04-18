open Batteries
open Cmdliner
open Dmlenu
open Candidate

module Parameter = struct
    let normal_background_default =
      try Sys.getenv "DMENU_NORMAL_BACKGROUND" with _ -> "#222222"
    let normal_foreground_default =
      try Sys.getenv "DMENU_NORMAL_FOREGROUND" with _ -> "#bbbbbb"
    let focus_background_default = try Sys.getenv "DMENU_FOCUS_BACKGROUND" with _ -> "#005577"
    let focus_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND" with _ -> "#eeeeee"
    let match_foreground_default = try Sys.getenv "DMENU_FOCUS_FOREGROUND_MATCH" with _ -> "#ff0000"

    let window_background = "#000000"

    let prompt =
      let doc = "Prompt to be displayed on the left of the input field" in
      Arg.(value & opt string "" & info ["p"; "prompt"] ~docv:"PROMPT" ~doc)

    let normal_background =
      let doc = "Normal background (for non focused elements)" in
      Arg.(value & opt string normal_background_default & info ["nb"] ~docv: "NB" ~doc)

    let focus_background =
      let doc = "Focus background color (for focused elements)" in
      Arg.(value & opt string focus_background_default & info ["fb"] ~docv: "fB" ~doc)

    let normal_foreground =
      let doc = "Normal foreground color (for non focused elements)" in
      Arg.(value & opt string normal_foreground_default & info ["nf"] ~docv: "NF" ~doc)

    let focus_foreground =
      let doc = "Focus foreground color (for focused elements)" in
      Arg.(value & opt string focus_foreground_default & info ["ff"] ~docv: "ff" ~doc)

    let match_foreground =
      let doc = "Color to display matches inside candidates" in
      Arg.(value & opt string match_foreground_default & info ["mf"] ~docv: "mf" ~doc)

    let window_background =
      let doc = "Color of the window background" in
      Arg.(value & opt string window_background & info ["wb"] ~docv: "wb" ~doc)

    let lines =
      let doc = "If set, display the candidates in lines" in
      Arg.(value & opt int 0 & info ["l"; "lines"] ~docv: "LINES" ~doc)

    let bottom =
      let doc = "If set, display the menu at the bottom of the screen" in
      Arg.(value & flag & info ["b"; "bottom"] ~doc)

    let stdin =
      let doc = "If set, read the candidates off stdin" in
      Arg.(value & flag & info ["s"; "stdin"] ~doc)
end

let reorder_dotfiles_end clist =
  let dotfiles, rest =
    List.partition (fun (s, _) ->
        try s.Candidate.display.[0] = '.'
        with Not_found -> false
      ) clist
  in
  rest @ dotfiles

let (^/) = Filename.concat

let run prompt stdin botbar focus_foreground focus_background normal_foreground
      normal_background match_foreground window_background lines =
  let () = Matching.(set_match_query_fun @@ subset ~case:false) in
  let () = Ordering.(set_reorder_matched_fun (reorder_dotfiles_end %> prefixes_first)) in
  let colors = Ui.Colors.({
    focus_foreground = Draw.Color.of_string_exn focus_foreground;
    focus_background = Draw.Color.of_string_exn focus_background;
    normal_foreground = Draw.Color.of_string_exn normal_foreground;
    normal_background = Draw.Color.of_string_exn normal_background;
    match_foreground = Draw.Color.of_string_exn match_foreground;
    window_background = Draw.Color.of_string_exn window_background;
  }) in
  let layout =
    match lines with
    | 0 -> State.SingleLine
    | n -> State.MultiLine n
  in
  let home = Sys.getenv "HOME" in
  let program =
    if stdin then Engine.singleton (Source.stdin ())
    else
      Engine.{
        sources = [
          Source.from_list [
            ("papiers", "evince", "");
            ("randr", "sh", "");
            ("j", "emacs", "");
          ];
          (Lazy.force Source.binaries)
        ];
        transition = fun c -> match c.display with
          | "papiers" -> Engine.singleton (Papiers.source (home ^/ "Papers"))
          | "randr" -> Engine.singleton (Source.files (home ^/ ".screenlayout"))
          | "j" -> Engine.singleton (Source.from_list [
              ("recherche", home ^/ "journal" ^/ "recherche", "");
              ("light", home ^/ "journal-light" ^/ "journal", "");
              ("lecture", home ^/ "journal" ^/ "pile-de-lecture", "");
              ("loisirs", home ^/ "journal" ^/ "loisirs", "")
            ])
          | _ -> Engine.iterate [ Source.files home ]
      }
  in
  let set_layout l st =
    App.{ st with state = { st.state with State.layout = l } } in
  let hook st =
    let entries =
      st.App.state.State.entries
      |> List.map (fun (_, c) -> c.display)
    in
    match entries with
    | "papiers" :: _
    | ":" :: _ -> set_layout (State.MultiLine 15) st
    | _ -> set_layout layout st
  in
  match App.run_list ~prompt ~layout ~topbar:(not botbar) ~colors ~hook program with
  | [] -> ()
  | prog :: _ as args -> Unix.execvp prog (Array.of_list args)

let info =
  let doc = "print a menu with customizable completion" in
  Term.info "dmlenu" ~version:"0.0" ~doc ~man:[]

let dmlenu =
  Parameter.(
    Term.(
      pure run $ prompt $ stdin $  bottom $ focus_foreground $ focus_background $
        normal_foreground $ normal_background $ match_foreground $
        window_background $ lines
    )
  )

let _ =
  match Term.eval (dmlenu, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
