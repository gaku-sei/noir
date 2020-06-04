type t

val fromString : string -> t

val fromStatus : Http.Status.t -> t

val fromJson : Js.Json.t -> t
