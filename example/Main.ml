open Core.Infix
open Noir

type homePayload = { hello : string }

let homePayloadToJson { hello } =
  Js.Json.object_ @@ Js.Dict.fromArray [| ("Hello ", Js.Json.string hello) |]

let homePipe = json @@ homePayloadToJson { hello = "world" }

let home =
  route "" >=> setHeader "X-Powered-By" "Noir" >=> setStatus `ok >=> homePipe

let hello =
  namespace "hello"
  >=> ( route "noir"
      >=> setHeader "X-Powered-By" "Noir"
      >=> text "Say hello to Noir!!"
      <|> capture (text <<< ( ^ ) "Say hello to ") )

let noContent = route "no-content" >=> status `noContent

let something =
  route "something" >=> setHeader "Something" "Else" >=> text "Something here"

let pipeline = home <|> noContent <|> something <|> hello

let () = run ~adapter:(module NativeAdapter) pipeline
