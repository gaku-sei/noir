type t =
  | String of string
  | Stream of Bindings.Node.Stream.Readable.t
  | Buffer of Bindings.Node.Buffer.t

let fromString string = String string

let fromReadableStream stream = Stream stream

let fromBuffer buffer = Buffer buffer

let fromStatus status = fromString @@ Http.Status.toMessage status

let fromJson json = fromString @@ Js.Json.stringify json

let toString = function
  | String string -> Js.Promise.resolve string
  | Stream stream -> Bindings.Node.Stream.Readable.consume stream
  | Buffer buffer ->
      Js.Promise.resolve
      @@ Bindings.Node.Buffer.toStringWithEncoding buffer `utf8
