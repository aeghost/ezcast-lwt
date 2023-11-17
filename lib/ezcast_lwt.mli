(** EZCAST-LWT
    @copyright None
    @author Matthieu GOSSET
    @maintainers
      Matthieu GOSSET <matthieu.gosset.dev@chapsvision.com>
    @purpose
      EZCast interface
*)

(** [type typ] declares the type of server that you may need `Broadcast or `Multicast *)
type typ = [ `Broadcast | `Multicast ]

(** [type env] let you know the last message and change the running status of the server *)
type env = { mutable history : string option; mutable running : bool; }

(** [make_env ()] create an empty env *)
val make_env : ?history:string option -> ?running:bool -> unit -> env

(** [cast ?verbose ?port ?addr ?buffer_size ?typ ?error_handler ?interval messages]
    Send [messages] over the nework using [typ] method on [addr] and port [port] sending [buffer_size], all messages will be sent with [interval] time between *)
val cast : ?verbose:bool -> ?buffer_size:int -> ?port:int -> ?addr:string -> ?typ:typ
  -> ?error_handler:(exn -> unit Lwt.t) -> ?interval:float -> string list -> unit Lwt.t

(** [server ?verbose ?port ?addr ?buffer_size ?typ ?error_handler ~callback ()]
    Declare a server listening multicast from [port] on [addr] using [typ] method;
    when a message is detected it will be given to [callback] and errors will be given to [error_handler] *)
val server : ?verbose:bool -> ?buffer_size:int -> ?port:int -> ?addr:string -> ?typ:typ -> ?env:env ->
  ?error_handler:(exn -> unit Lwt.t) -> callback:(env:env -> string -> unit Lwt.t) -> unit -> unit Lwt.t
