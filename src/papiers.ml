open Batteries

let query_of_string s =
  let words = String.nsplit ~by:" " s
              |> List.filter (not % String.is_empty) in
  List.map (fun w ->
      let open Papierslib in
      match String.Exceptionless.split ~by:":" w with
      | Some ("title", s) | Some ("ti", s) -> Query.Title s
      | Some ("a", s) | Some ("au", s) | Some ("author", s) -> Query.Author s
      | Some ("s", s) | Some ("src", s) | Some ("source", s) -> Query.Source s
      | Some ("ta", s) | Some ("tag", s) -> Query.Tag s
      | Some ("la", s) | Some ("lang", s) -> Query.Lang s
      | _ -> Query.String w
    ) words

let dummy_matching disp : Matching.t = fun _ -> Some [(false, 0, String.length disp)]

let source repo =
  let repo_path = Papierslib.Path.of_string repo in
  if Papierslib.Db.find repo_path <> Some repo_path then
    Source.from_list []
  else
    let db = Papierslib.Db.load repo_path in
    let compute db query_s =
      let query = query_of_string query_s in
      let candidates =
        Papierslib.Cmd.search db query
        |> List.filter_map
          (function doc_id ->
             let open Papierslib in
             let doc = Document.get db ~rel_paths:false doc_id in
             let contents = doc.Document.content in
             match contents.Document.source with
             | [] -> None
             | src :: _ ->
               let display = contents.Document.name in
               Candidate.make
                 ~matching_function:(dummy_matching display)
                 ~real:(Source.to_string src)
                 ~doc:(Printf.sprintf "%s | %s"
                         (String.join ", " contents.Document.authors)
                         (String.join ", " contents.Document.tags))
                 display
               |> Option.some)
      in
      (db, candidates)
    in
    Source.S { Source.delay = true; default_state = db; compute }
