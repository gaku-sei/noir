open Util.Infix

type request = {
  headers : string Js.Dict.t;
  url : string;
  verb : string; [@bs.as "method"]
}

external requestToStream : request -> Bindings.Node.Stream.Readable.t
  = "%identity"

let%private readVerb =
  Js.String.toLowerCase >>> function
  | "head" -> `head
  | "options" -> `get
  | "trace" -> `trace
  | "connect" -> `connect
  | "get" -> `get
  | "post" -> `post
  | "put" -> `put
  | "delete" -> `delete
  | "patch" -> `patch
  | _ -> `get

type response = { mutable statusCode : int; mutable statusMessage : string }

(* Support streams and buffers  *)
external write : response -> string -> unit = "write" [@@bs.send]

external _end : response -> unit = "end" [@@bs.send]

external setHeader : response -> string -> string -> unit = "setHeader"
  [@@bs.send]

type t

external createServer' : (request -> response -> unit) -> t = "createServer"
  [@@bs.module "http"]

external listen' : t -> int -> unit = "listen" [@@bs.send]

let listen f =
  let server =
    createServer' @@ fun ({ headers; url; verb } as request) response ->
    f
      (Request.make
         ~body:(Serializable.fromReadableStream @@ requestToStream request)
         ~headers ~pathName:url ~url ~verb:(readVerb verb) ())
    |> Js.Promise.then_ (fun ({ body; headers; status } : Response.t) ->
           (* Set status *)
           response.statusCode <- Http.Status.toCode status;
           response.statusMessage <- Http.Status.toMessage status;
           (* Set headers *)
           headers |> Js.Dict.entries
           |> Js.Array.forEach (fun (key, value) ->
                  setHeader response key value);
           (* FIXME: Should disambiguate strings, streams, and buffers *)
           Serializable.toString
           @@ Belt.Option.getWithDefault body
           @@ Serializable.fromStatus status)
    |> Js.Promise.then_ (fun body ->
           (* Write response content *)
           write response body;
           (* End request *) _end response;
           Js.Promise.resolve ())
    |> ignore
  in

  listen' server 3000
