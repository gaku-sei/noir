type t = string Js.Dict.t

let set key value headers =
  Js.Dict.fromArray
  @@ Belt.Array.concat [| (key, value) |]
  @@ Js.Dict.entries headers
