open Core.Infix

type t = Serializable : 'a -> t [@@unboxed]

let make x = Serializable x

let fromString = make

let fromStatus = fromString <<< Http.Status.toMessage

let fromJson = fromString <<< Js.Json.stringify
