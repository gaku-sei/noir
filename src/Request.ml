(* Handle url params :id *)
(* Handle body *)
type t = { headers : Headers.t; pathName : string; url : string }

let make ?(headers = Js.Dict.empty ()) ~pathName ~url () =
  { headers; pathName; url }
