(** CLIENT
    @author Matthieu GOSSET
    @purpose
      Minimalist client
*)

let get_path i = try int_of_string @@ Array.get Sys.argv i
  with _ -> failwith "Enter a port to bind a peer"

let messages = [
  `Message "text";
  `Type (`Dummy);
  `Stop
]

type c = {
  listen: int;
  write: int;
}

let configuration = {
  listen = get_path 1;
  write = get_path 2;
}

let log = print_endline

let send_messages () =
  Ezcast_lwt.cast ~interval:0.1
  @@ List.mapi (fun i m ->
      log ("Sending : " ^ string_of_int i);
      Protocol.to_string m
    ) messages

let on_message ~(env : Ezcast_lwt.env) = function
  | `Ack ->
    log "Server Ack";
    Lwt.pause ()
  | `Stop ->
    log "Stopping...";
    env.running <- true;
    Lwt.pause ()
  | _ ->
    log "Unkown Message";
    Lwt.pause ()

let ack_server_messages () =
  Ezcast_lwt.server ~callback:(fun ~env s -> Protocol.of_string s |> on_message ~env) ~port:configuration.listen ()

let main () =
  let () = log @@ "Client Starting with [listening: " ^ string_of_int configuration.listen ^ "] and [writing: " ^ string_of_int configuration.write ^ "]" in
  let%lwt () = Lwt.join [
      ack_server_messages ();
      let%lwt () = Lwt.pause () in send_messages ()
    ] in
  let () = log "Client Stopping" in
  Lwt.return_unit

let () = Lwt_main.run @@ main ()

