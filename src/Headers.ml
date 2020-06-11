type t = string Js.Dict.t

let get key headers = Js.Dict.get headers key

let set key value headers =
  Js.Dict.fromArray
  @@ Belt.Array.concat [| (key, value) |]
  @@ Js.Dict.entries headers
