type t = string Js.Dict.t

let set key value headers =
  headers |. Js.Dict.entries
  |. Belt.Array.concat [| (key, value) |]
  |. Js.Dict.fromArray
