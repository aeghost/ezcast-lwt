(** EZCAST-LWT
    @copyright 2023 MIT -- NO RIGHTS RESERVED. Deal with it !
    @author Matthieu GOSSET
    @maintainers
      Matthieu GOSSET <matthieu.gosset.dev@outlook.com>
    @purpose
      Manage easily informations sharing through Multicast OR Broadcast
*)

type typ = [`Broadcast | `Multicast]

type env = {
  mutable history: string option;
  mutable running: bool;
}

let make_env ?(history = None) ?(running = true) () : env =
  { history; running }

type state = [ `Started | `Stopped ]

type t = {
  mutable addr: string;
  mutable port: int;
  mutable typ: typ;
  mutable buffer_size: int;
  mutable callback: env:env -> string -> unit Lwt.t;
  mutable handler: exn -> unit Lwt.t;
  mutable socket: Lwt_unix.file_descr option;
  mutable state: state;
  mutable verbose: bool;
}
[@@ deriving make]

let make
    ?(addr =  "225.10.0.42")
    ?(port =  3000)
    ?(typ =  `Multicast)
    ?(buffer_size =  1024)
    ?(callback =  fun ~env _ -> let _ = env in Lwt.return_unit)
    ?(handler =  fun _ -> Lwt.return_unit)
    ?(socket =  None)
    ?(state =  `Stopped)
    ?(verbose =  false) () : t =
  { addr; port; typ; buffer_size; callback; handler; socket; state; verbose; }

module Multicast = struct
  exception Not_initialized
  exception Corrupted_message

  let get_addr c = Unix.inet_addr_of_string c.addr

  let close c s =
    Lwt_unix.mcast_drop_membership s (get_addr c);
    Lwt_unix.close s

  let open_sock c =
    let sock = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    Lwt_unix.(setsockopt sock SO_REUSEADDR true);
    Lwt_gc.finalise_or_exit (close c) sock;
    Lwt.return (Some sock)

  let send c (m: string) =
    let socket = match c.socket with
        None -> raise Not_initialized
      | Some socket -> socket in
    if c.verbose then Fmt.pr "[Multicast] Updating all on %s:%i@." c.addr c.port;
    let data = String.to_bytes m in
    let len = Bytes.length data in
    let sockaddr = Unix.(ADDR_INET (get_addr c, c.port)) in
    let%lwt size = Lwt_unix.sendto socket data 0 len [] sockaddr in
    if size <> len then (
      if c.verbose then Fmt.pr "[Multicast] Error sending update@.";
      raise Corrupted_message
    ) else Lwt.return_unit

  let server_unsafe ~env c () =
    let socket =
      match c.socket with
        None -> raise Not_initialized
      | Some socket -> socket
    in

    if c.verbose then Fmt.pr "[Multicast] Listening for updates...@.";

    let sockaddr = Unix.(ADDR_INET (Unix.inet_addr_any, c.port)) in
    let%lwt () = try
        Lwt_unix.bind socket sockaddr;%lwt
        Lwt_unix.mcast_add_membership socket (get_addr c);
        Lwt.return_unit
      with e ->
        if c.verbose then Fmt.pr "Can't bind %a@." Fmt.exn e;
        raise e
    in

    let buffer = Bytes.create c.buffer_size in

    let on_message peer port =
      match Bytes.(to_string buffer) with
      | content ->
        let is_new_op = match env.history with
          | None -> true
          | Some last_content when content <> last_content -> true
          | _ -> false in
        if is_new_op then begin
          if c.verbose then Fmt.pr "[Multicast] Updating from [peer: %s] on [port: %i]@." (Unix.string_of_inet_addr peer) port;
          env.history <- Some content;
          c.callback ~env content
        end else Lwt.return_unit
      | exception e ->
        if c.verbose then Fmt.pr "[Multicast] Bad message [error: %a]@." Fmt.exn e;
        c.handler e
    in

    while%lwt env.running do (
      c.state <- `Started;
      match%lwt Lwt_unix.recvfrom socket buffer 0 c.buffer_size [] with
      | _, Unix.ADDR_INET (peer, port) ->
        let%lwt () = on_message peer port in
        Lwt.return_unit
      | _ ->
        if c.verbose then Fmt.pr "[Multicast] Who is talking to me ?@.";
        Lwt_unix.sleep 1.;%lwt
        Lwt.return_unit
      | exception e ->
        if c.verbose then Fmt.pr "[Multicast] Bad read [error: %a]@." Fmt.exn e;
        Lwt_unix.sleep 1.;%lwt
        Lwt.return_unit
    ) done;%lwt
    c.state <- `Stopped;
    Lwt.return_unit

end

module Broadcast = struct

end

let cast ?verbose ?buffer_size ?port ?addr ?typ ?(error_handler = fun _ -> Lwt.return_unit) ?interval messages =
  let c = make () in
  let select e e' = match e with None -> e' | Some v -> v in
  c.buffer_size <- select buffer_size c.buffer_size;
  c.port <- select port c.port;
  c.addr <- select addr c.addr;
  c.typ <- select typ c.typ;
  c.verbose <- select verbose c.verbose;
  let%lwt socket = match c.typ with _ -> Multicast.open_sock c in
  c.socket <- socket;

  let for_all_messages = function
    | m when c.typ = `Multicast ->
      let%lwt () = Multicast.send c m in
      (match interval with
         None -> Lwt.pause ()
       | Some i -> Lwt_unix.sleep i)
    | _ -> Lwt.return_unit
  in

  try%lwt try
      Lwt_list.iter_s for_all_messages messages
    with e -> Lwt.fail e
  with e ->
    if c.verbose then Fmt.pr "[Multicast] Updating failed [error: %a]@." Fmt.exn e;
    error_handler e;%lwt
    Lwt.return_unit

let server ?verbose ?buffer_size ?port ?addr ?typ ?(env = make_env ())
    ?(error_handler = fun _ -> Lwt.return_unit)
    ~callback () : unit Lwt.t =
  let c = make () in
  let select e e' = match e with None -> e' | Some v -> v in
  c.buffer_size <- select buffer_size c.buffer_size;
  c.port <- select port c.port;
  c.addr <- select addr c.addr;
  c.typ <- select typ c.typ;
  c.callback <- callback;
  c.handler <- error_handler;
  c.verbose <- select verbose c.verbose;
  let%lwt socket = match c.typ with _ -> Multicast.open_sock c in
  c.socket <- socket;

  if c.verbose then Fmt.pr "[Multicast] Updating service launched on %i@." c.port;

  let server =
    match c.typ with _ ->
      Multicast.server_unsafe ~env
  in

  try%lwt try
      Lwt.async (server c);
      Lwt.return_unit
    with e -> Lwt.fail e
  with e ->
    if c.verbose then Fmt.pr "[Multicast] Updating failed [error: %a]@." Fmt.exn e;
    Lwt.return_unit
