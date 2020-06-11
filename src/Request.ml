(* Handle url params :id *)
type t = {
  body : Serializable.t option;
  headers : Headers.t;
  pathName : string;
  url : string;
  verb : Http.Verb.t;
}
[@@bs.deriving accessors]

let make ?(headers = Js.Dict.empty ()) ?body ~pathName ~url ~verb () =
  { body; headers; pathName; url; verb }

let empty = make ~pathName:"" ~url:"" ~verb:`get ()
