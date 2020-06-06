open Core.Infix
open Noir

type homePayload = { hello : string }

let homePayloadToJson { hello } =
  Js.Json.object_ @@ Js.Dict.fromArray [| ("Hello ", Js.Json.string hello) |]

let home =
  route ""
  >=> setHeader "X-Powered-By" "Noir"
  >=> setStatus `ok
  >=> json @@ homePayloadToJson { hello = "world" }

let hello =
  namespace "hello"
  >=> ( route "noir"
      >=> setHeader "X-Powered-By" "Noir"
      >=> text "Say hello to Noir!!"
      <|> capture (text <<< ( ^ ) "Say hello to ") )

let noContent = route "no-content" >=> status `noContent

let something =
  route "something" >=> setHeader "Something" "Else" >=> text "Something here"

let withPayload = route "with-payload" >=> verb `post >=> text "I heard you"

let pipeline = home <|> noContent <|> something <|> hello <|> withPayload

let () = run ~adapter:(module NativeAdapter) pipeline
