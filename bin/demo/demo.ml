(** DEMO
    @copyright None
    @author Matthieu GOSSET
    @maintainers
      Matthieu GOSSET <matthieu.gosset.dev@chapsvision.com>
    @purpose
      Quick exemple
      Demo will follow some good practices
        of abstrating communication protocols through typing
*)
let port = try int_of_string @@ Array.get Sys.argv 1
  with _ -> failwith "Enter a port to cast"

let i = ref 0
let incr () = i := !i + 1
let print (s: string) =
  incr ();
  print_endline ((string_of_int !i) ^ ". " ^ s)

(* Defining a type protocol will let you do the management and the parsing at different places.
   - It will assure you a complete cover of the protocol
   - It will reduce side effects
*)
module Protocol = struct
  type t = [
      `Any of string
    | `Stop
  ]

  let of_string : string -> t = function
    | s when Str.string_match (Str.regexp "stop") s 0 -> `Stop
    | s -> `Any s

  let manage (env: Ezcast_lwt.env) : t -> _ = function
    | `Stop ->
      env.running <- false;
      Lwt.pause ()
    | `Any s ->
      print s;
      (* Bandwidth.(read := !read + String.(length s)); *)
      Lwt.return_unit
end

let callback ~env s = Protocol.(of_string s |> manage env)

let main () =
  let () = print "Demo starting" in
  let%lwt () = Lwt.join [
      Ezcast_lwt.server ~port ~callback ();
      (
        Lwt.pause ();%lwt
        Ezcast_lwt.(cast ~interval:0.000001 [ "test"; "aa"; "b"; "stop" ])
      ) ] in
  let () = print "Demo stopping" in
  Lwt.return_unit

let () = main () |> Lwt_main.run
