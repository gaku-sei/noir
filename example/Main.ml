open Util.Infix
open Core

type homePayload = { hello : string } [@@decco.encode]

type payload = { name : string; age : int } [@@decco.decode]

let decodeWith decoder =
  Js.Json.parseExn >>> decoder >>> function
  | Ok ok -> Ok ok
  | Error { Decco.message; path } ->
      Error (Serializable.fromString @@ path ^ " error: " ^ message)

let home =
  route ""
  >=> setHeader "X-Powered-By" "Noir"
  >=> setStatus `ok
  >=> json @@ homePayload_encode { hello = "world" }

let hello =
  namespace "hello"
  >=> ( route "noir"
      >=> setHeader "X-Powered-By" "Noir"
      >=> text "Say hello to Noir!!"
      <|> capture (text <<< ( ^ ) "Say hello to ") )

let noContent = route "no-content" >=> status `noContent

let something =
  route "something" >=> setHeader "Something" "Else" >=> text "Something here"

let withPayload =
  route "with-payload"
  >=> verb `post
  >=> payload (decodeWith payload_decode) (fun { age; name } ->
          text @@ "Seems " ^ name ^ " is " ^ string_of_int age ^ " years old")

let pipeline = home <|> noContent <|> something <|> hello <|> withPayload

let () = run ~adapter:(module NativeAdapter) pipeline
