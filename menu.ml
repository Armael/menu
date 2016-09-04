open Batteries
open Cmdliner
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
      Arg.(value & opt string match_foreground_default & info ["ff"] ~docv: "mf" ~doc)

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

let run prompt stdin botbar focus_foreground focus_background normal_foreground
      normal_background match_foreground window_background lines =
  let () = Matching.(set_match_query_fun @@ subset ~case:false) in
  let () = Ordering.(set_reorder_matched_fun prefixes_first) in
  let colors = { X.Colors.focus_foreground; focus_background;
                 normal_foreground; normal_background; match_foreground; window_background } in
  let layout =
    match lines with
    | 0 -> State.SingleLine
    | n -> State.MultiLine n
  in
  let program =
    if stdin then Engine.singleton (Source.stdin ())
    else
      Engine.{
        sources = [
          Source.from_list [
            ("twitch", "mpv", "");
            ("papiers", "xdg-open", "");
          ];
          Source.binaries
        ];
        transition = fun c -> match c.display with
          | "twitch" -> Engine.singleton (Twitch.source "twitch_user")
          | "papiers" -> Engine.singleton (Papiers.source "/home/armael/Papers")
          | _ -> Engine.iterate [ Source.files (Sys.getenv "HOME") ]
      }
  in
  let set_layout l st =
    Dmlenu.{ st with state = { st.state with State.layout = l } } in
  let hook st =
    let entries =
      st.Dmlenu.state.State.entries
      |> List.map (fun (_, c) -> c.display)
    in
    match entries with
    | "twitch" :: _ -> set_layout (State.MultiLine 10) st
    | "papiers" :: _ -> set_layout (State.MultiLine 10) st
    | _ -> set_layout layout st
  in
  match Dmlenu.run_list ~prompt ~layout ~topbar:(not botbar) ~colors ~hook program with
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
