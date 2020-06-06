open Util.Infix

type t = string Js.Dict.t

let set key value =
  Js.Dict.fromArray <<< Belt.Array.concat [| (key, value) |] <<< Js.Dict.entries
