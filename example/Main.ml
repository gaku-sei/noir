open Noir

let home =
  route ""
  >=> setHeader "X-Powered-By" "Noir"
  >=> setContentType `json
  >=> setStatus `ok
  >=> text "{\"hello\": \"idp\"}"

let hello =
  namespace "hello"
  >=> ( route "noir"
      >=> setHeader "X-Powered-By" "Noir"
      >=> text "Hello Noir!!"
      <|> (route "unknown" >=> text "Hello?") )

let noContent = route "no-content" >=> setStatus `noContent

let something =
  route "something" >=> setHeader "Something" "Else" >=> text "Hello!"

let pipeline = home <|> hello <|> something <|> noContent

let () = run ~adapter:(module NativeAdapter) pipeline
