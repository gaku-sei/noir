type t

val fromString : string -> t

val fromReadableStream : Bindings.Node.Stream.Readable.t -> t

val fromBuffer : Bindings.Node.Buffer.t -> t

val fromStatus : Http.Status.t -> t

val fromJson : Js.Json.t -> t

val toString : t -> string Js.Promise.t
