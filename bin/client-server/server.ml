(** SERVER
    @author Matthieu GOSSET
    @purpose
      Minimalist server
*)

type c = {
  listen: int;
  write: int;
}

let get_path i = try int_of_string @@ Array.get Sys.argv i
  with _ -> failwith "Enter a port to bind a peer"

let log = print_endline

let configuration: c = {
  listen = get_path 1;
  write = get_path 2;
}

let respond m =
  log "Responding";
  Ezcast_lwt.cast ~port:configuration.write [Protocol.to_string m]

let on_message ~(env: Ezcast_lwt.env) = function
  | `Message m ->
    respond `Ack;%lwt
    log ("Client has sent " ^ m);
    Lwt.return_unit
  | `Type `Dummy ->
    respond `Ack;%lwt
    log "Client has sent type `Dummy";
    Lwt.return_unit
  | `Stop ->
    respond `Stop;%lwt
    log "Server Stopping...";
    env.running <- false;
    Lwt.return_unit
  | `Error ->
    respond `Error;%lwt
    log "Client send a bad formatted message...";
    Lwt.return_unit
  | _ ->
    respond `Ack;%lwt
    log "Got unkown message";
    Lwt.return_unit

let serve () =
  Ezcast_lwt.server ~verbose:true ~callback:(fun ~env s -> Protocol.of_string s |> on_message ~env) ~port:configuration.listen ()

let main () =
  let () = log @@ "Server Starting with [listening: " ^ string_of_int configuration.listen ^ "] and [writing: " ^ string_of_int configuration.write ^ "]" in
  let%lwt () = Lwt.join [
      serve ()
    ] in
  let () = print_endline "Server Stopping" in
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
