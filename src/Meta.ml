type t = { currentNamespace : string }

let make () = { currentNamespace = "" }

let mapCurrentNamespace f { currentNamespace } =
  { currentNamespace = f currentNamespace }
