type 'a t = [
    `Stop
  | `Message of string
  | `Type of 'a
  | `Ack
  | `Error
]

let of_string : string -> 'a t =
  function
  | "stop" -> `Stop
  | "ack" -> `Ack
  | "err" -> `Error
  | s when String.get s 0 = 'm' -> `Message String.(sub s 1 (length s - 1))
  | s -> try `Type (Marshal.from_string (String.trim s) 0) with _ -> `Error

let to_string : 'a t -> string = function
  | `Message s -> "m" ^ s
  | `Stop -> "stop"
  | `Ack -> "ack"
  | `Error -> "err"
  | `Type a -> Marshal.to_string a []
