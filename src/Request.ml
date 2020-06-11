(* Handle url params :id *)
type t = {
  body : Serializable.t option;
  headers : Headers.t;
  pathName : string;
  url : string option;
  verb : Http.Verb.t;
}
[@@bs.deriving accessors]

let make ?(headers = Js.Dict.empty ()) ?body ?url ~pathName ~verb () =
  { body; headers; pathName; url; verb }

let empty = make ~pathName:"" ~verb:`get ()

let setUrl url request = { request with url }
