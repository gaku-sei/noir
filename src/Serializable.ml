type t =
  | String of string
  | Stream of Bindings.Node.Stream.Readable.t
  | Buffer of Bindings.Node.Buffer.t

let fromString string = String string

let fromStream stream = Stream stream

let fromBuffer buffer = Buffer buffer

let fromStatus status = fromString @@ Http.Status.toMessage status

let fromJson json = fromString @@ Js.Json.stringify json

let length = function
  | String string -> Some (Js.String.length string)
  | Stream _ -> None
  | Buffer buffer -> Some (Bindings.Node.Buffer.length buffer)
