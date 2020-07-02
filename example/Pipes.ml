open Util
open Core

type config = { db : string }

type homePayload = { hello : string } [@@decco.encode]

type payload = { name : string; age : int } [@@decco.decode]

let decodeWith decoder =
  let tryParse string = try Js.Json.parseExn string with _ -> Js.Json.null in
  tryParse >> decoder >> function
  | Ok ok -> Ok ok
  | Error { Decco.message; path } ->
      Error
        ( Serializable.fromString
        @@ (if path = "" then "" else path ^ " ")
        ^ "error: " ^ message )

let home =
  route ""
  >=> setHeader "X-Powered-By" "Noir"
  >=> setStatus `ok
  >=> json @@ homePayload_encode { hello = "world" }

let hello =
  route "hello"
  >=> ( route "noir"
      >=> setHeader "X-Powered-By" "Noir"
      >=> text "Say hello to Noir!!"
      <|> capture (text << ( ^ ) "Say hello to ") )

let noContent = route "no-content" >=> status `noContent

let something =
  route "something" >=> setHeader "Something" "Else" >=> text "Something here"

let withPayload =
  route "with-payload"
  >=> verb `post
  >=> payload (decodeWith payload_decode) (fun { age; name } ->
          text @@ "Seems that " ^ name ^ " is " ^ string_of_int age
          ^ " years old")

let pipeline : config Pipe.t =
  home <|> noContent <|> something <|> hello <|> withPayload
