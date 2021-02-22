open Core

type config = {db: string}

@decco.encode type homePayload = {hello: string}

@decco.decode type payload = {name: string, age: int}

let jsonParse = string =>
  try Js.Json.parseExn(string) catch {
  | _ => Js.Json.null
  }

let decodeWith = (decoder, payload) =>
  switch payload->jsonParse->decoder {
  | Ok(ok) => Ok(ok)
  | Error({Decco.message: message, path}) =>
    let path = path == "" ? "" : `${path} `
    Error(Serializable.fromString(`${path}error: ${message}`))
  }

let home =
  route("")
  ->pipe(setHeader("X-Powered-By", "Noir"))
  ->pipe(setStatus(#ok))
  ->pipe(json(homePayload_encode({hello: "world"})))

let hello =
  route("hello")->pipe(
    oneOf([
      route("noir")->pipe(setHeader("X-Powered-By", "Noir"))->pipe(text("Say hello to Noir!!")),
      capture(name => text(`Say hello to ${name}`)),
    ]),
  )

let noContent = route("no-content")->pipe(status(#noContent))

let something =
  route("something")->pipe(setHeader("Something", "Else"))->pipe(text("Something here"))

let withPayload =
  route("with-payload")
  ->pipe(verb(#post))
  ->pipe(
    payload(decodeWith(payload_decode), ({age, name}) =>
      text(`It seems that ${name} is ${Belt.Int.toString(age)} years old`)
    ),
  )

let pipeline: Pipe.t<config> = oneOf([home, noContent, something, hello, withPayload])

let listen: (Request.t => Js.Promise.t<Response.t>) => unit = _ => ()
