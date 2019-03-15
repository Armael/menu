open Batteries
open Lwt
open Cohttp
open Cohttp_lwt_unix

let headers = Header.of_list [
    ("Accept", "application/vnd.twitch.v3+json");
    ("Client-ID", "XXXX")
  ]

let limit = 100

let access_kraken ?(payload = []) cmd =
  let url = "https://api.twitch.tv/kraken" ^ cmd
            |> Uri.of_string
            |> flip Uri.add_query_params payload
  in
  let%lwt (rcode, body) = Client.get ~headers url in
  if Code.(is_success (code_of_status rcode.Response.status)) then
    let%lwt body_s = Cohttp_lwt.Body.to_string body in
    Lwt.return (Some (Yojson.Basic.from_string body_s))
  else
    Lwt.return None

type channel = {
  name : string;
  display_name : string;
  game : string option;
  status : string;
  url : string;
}

let channel_of_json j =
  let open Yojson.Basic.Util in
  let c = member "channel" j in
  { name = member "name" c |> to_string;
    display_name = member "display_name" c |> to_string;
    game = member "game" c |> to_string_option;
    status = member "status" c |> to_string;
    url = member "url" c |> to_string;
  }

let get_followed_channels ?payload ~nick () =
  let cmd = "/users/" ^ nick ^ "/follows/channels" in
  let%lwt json = access_kraken ?payload cmd in
  begin match json with
    | Some j ->
      begin try
          Yojson.Basic.Util.(member "follows" j |> to_list)
          |> List.map channel_of_json
          |> Option.some
        with Yojson.Basic.Util.Type_error (_, _) -> None
      end
    | None -> None
  end |> Lwt.return

let check_if_online ~chans () =
  let streams = ref (
      List.fold_left (fun map chan ->
          Map.add chan false map)
        Map.empty chans
    ) in

  let rec query chans chans_len =
    let cs, rest =
      if chans_len <= limit then
        chans, []
      else
        List.split_at limit chans
    in
    let payload = [
      ("channel", cs);
      ("limit", [string_of_int limit]);
      ("offset", ["0"])
    ] in
    let%lwt json = access_kraken ~payload "/streams" in
    begin match json with
      | Some j ->
        Yojson.Basic.Util.(
          member "streams" j |> to_list
          |> List.map (member "channel" %> member "name" %> to_string)
          |> List.iter (fun n -> streams := Map.add n true !streams)
        )
      | None -> raise Exit
    end;
    if rest = [] then Lwt.return ()
    else query rest (chans_len - limit)
  in
  let%lwt success =
    try query chans (List.length chans) >|= fun () -> true
    with Yojson.Basic.Util.Type_error (_, _) -> Lwt.return false
  in
  Lwt.return (if success then Some !streams else None)

let get_online_followed_channels ~nick () =
  let%lwt followed = get_followed_channels ~nick () in
  match followed with
  | None -> Lwt.return None
  | Some followed ->
    let%lwt online_map = check_if_online
        ~chans:(List.map (fun c -> c.name) followed)
        ()
    in
    match online_map with
    | None -> Lwt.return None
    | Some online_map ->
      List.filter (fun c -> Map.find c.name online_map) followed
      |> Option.some
      |> Lwt.return

let source nick =
  let source_it_of_channel =
    function { name = _; display_name; game; status; url } ->
      (display_name, url,
       match game with
       | None -> Printf.sprintf "%s" status
       | Some game -> Printf.sprintf "playing %s: %s" game status)
  in
  Lazy.from_fun (fun () ->
      let chans_lwt =
        let%lwt chans = get_online_followed_channels ~nick () in
        Lwt.return (List.map source_it_of_channel (chans |? []))
      in
      Lwt_main.run (
        pick [
          (Lwt_unix.sleep 5.0 >|= fun () -> []);
          chans_lwt
        ]
      )
    )
  |> Source.from_list_lazy
