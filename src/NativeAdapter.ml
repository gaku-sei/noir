type request = {
  headers : string Js.Dict.t;
  pathName : string; [@bs.as "url"]
  verb : string; [@bs.as "method"]
}

external requestToStream : request -> Bindings.Node.Stream.Readable.t
  = "%identity"

let%private readVerb verb =
  verb |> Js.String.toLowerCase |> function
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

module ListenOptions = struct
  type t = { host : string; port : int }
end

external listen' : t -> ListenOptions.t -> (unit -> unit) Js.Nullable.t -> unit
  = "listen"
  [@@bs.send]

let make ?(host = "127.0.0.1") ?(port = 3000) ?onListen () =
  ( module struct
    let listen f =
      let server =
        createServer'
        @@ fun ({ headers; pathName; verb } as request) response ->
        f
          (Request.make
             ~body:
               (Serializable.fromStream @@ requestToStream request)
               (* As stated here https://nodejs.org/api/http.html#http_message_url the native http(s) modules'
                  requests (incoming message) will not compute the url by default, but it's pretty easy to add a pipe
                  for that if needed. Url is then always None *)
             ~headers ~pathName ~verb:(readVerb verb) ())
        |> Js.Promise.then_ (fun ({ body; headers; status } : Response.t) ->
               (* When reaching this point the status should be set, but it can be assumed as not found *)
               let status = Belt.Option.getWithDefault status `notFound in
               (* Set status *)
               response.statusCode <- Http.Status.toCode status;
               response.statusMessage <- Http.Status.toMessage status;
               (* Set headers *)
               headers |> Js.Dict.entries
               |> Js.Array.forEach (fun (key, value) ->
                      setHeader response key value);
               match body with
               | None -> Js.Promise.resolve @@ Http.Status.toMessage status
               | Some (String string) -> Js.Promise.resolve string
               | Some (Buffer buffer) ->
                   Js.Promise.resolve
                   @@ Bindings.Node.Buffer.toStringWithEncoding buffer `utf8
               | Some (Stream stream) ->
                   Bindings.Node.Stream.Readable.consume stream)
        |> Js.Promise.then_ (fun body ->
               (* Write response content *)
               write response body;
               (* End request *) _end response;
               Js.Promise.resolve ())
        |> ignore
      in

      listen' server { host; port } @@ Js.Nullable.fromOption onListen
  end : Adapter.Type )
