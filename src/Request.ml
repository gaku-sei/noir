(* Handle url params :id *)
type t = {
  body : Serializable.t option;
  headers : Headers.t;
  pathName : string;
  url : string;
}
[@@bs.deriving accessors]

let make ?(headers = Js.Dict.empty ()) ?body ~pathName ~url () =
  { body; headers; pathName; url }
