open Pipe

let%private currentPathPart
    { Context.request = { pathName }; meta = { currentNamespace } } =
  let from = Js.String.length currentNamespace + 1 in
  let to_ = pathName |> Js.String.indexOfFrom "/" from in
  Js.String.substring pathName ~from
    ~to_:(if to_ < 0 then Js.String.length pathName else to_)

let%private handlePayload f decodedBody ctx =
  match decodedBody with
  | Ok payload -> f payload ctx
  | Error errors ->
      pure
      @@ Context.mapResponse (Response.setStatus `badRequest)
      @@ Context.mapResponse (Response.setBody errors) ctx

let choose pipes ctx =
  Belt.List.reduceReverse pipes (pure ctx) (fun acc pipe -> pipe ctx <|> acc)

let setContentType contentType ctx =
  pure
  @@ Context.mapResponse
       ( Response.mapHeaders @@ Headers.set "Content-Type"
       @@ Http.ContentType.show contentType )
       ctx

let setContentLength contentLength ctx =
  if contentLength < 0 then pure ctx
  else
    pure
    @@ Context.mapResponse
         ( Response.mapHeaders
         @@ Headers.set "Content-Length"
         @@ string_of_int contentLength )
         ctx

let setStatus status ctx =
  pure @@ Context.mapResponse (Response.setStatus status) ctx

let setHeader key value ctx =
  if key = "" || value = "" then pure ctx
  else
    pure
    @@ Context.mapResponse (Response.mapHeaders (Headers.set key value)) ctx

let setHeaders headers ctx =
  pure
  @@ Context.mapResponse
       ( Response.setHeaders @@ Js.Dict.fromArray
       @@ Js.Array.filter (fun (key, value) -> key <> "" && value <> "")
       @@ Js.Dict.entries headers )
       ctx

let setCookie ?expires ?maxAge ?domain ?path ?(secure = false)
    ?(httpOnly = false) ?sameSite name content =
  setHeader "Set-Cookie"
  @@ Belt.List.reduce
       [
         (name, Some content);
         ("Expires", Belt.Option.map expires Js.Date.toUTCString);
         ("Max-Age", Belt.Option.map maxAge string_of_int);
         ("Domain", domain);
         ("Path", path);
         ("Secure", if secure then Some "" else None);
         ("HttpOnly", if httpOnly then Some "" else None);
         ( "SameSite",
           Belt.Option.map sameSite @@ function
           | `strict -> "Strict"
           | `lax -> "Lax"
           | `none -> "None" );
       ]
       ""
  @@ fun acc (key, value) ->
  acc
  ^
  match value with
  | None -> ""
  | Some "" when acc = "" -> key
  | Some "" -> "; " ^ key
  | Some value when acc = "" -> key ^ "=" ^ value
  | Some value -> "; " ^ key ^ "=" ^ value

let resolveUrl ?(secure = true)
    ({ Context.request = { headers; pathName } } as ctx) =
  let host = Headers.get "host" headers in
  let protocol = if secure then "https://" else "http://" in
  let url =
    Belt.Option.map
      (Belt.Option.flatMap host (fun host ->
           Bindings.Node.Url.make pathName (protocol ^ host)))
      Bindings.Node.Url.href
  in
  pure @@ (Context.mapRequest @@ Request.setUrl url) ctx

let option pipe option ctx =
  match option with None -> pure ctx | Some x -> pipe x ctx

let payload decode f ({ Context.request = { body } } as ctx) =
  match body with
  | None -> setStatus `badRequest ctx
  | Some (String body) -> (handlePayload f @@ decode body) ctx
  | Some (Buffer body) ->
      ( handlePayload f @@ decode
      @@ Bindings.Node.Buffer.toStringWithEncoding body `utf8 )
        ctx
  | Some (Stream body) ->
      Async
        ( Bindings.Node.Stream.Readable.consume body
        |> Js.Promise.then_ (fun body ->
               match decode body with
               | Error errors ->
                   Js.Promise.resolve
                   @@ Ok
                        ( Context.mapResponse (Response.setStatus `badRequest)
                        @@ Context.mapResponse (Response.setBody errors) ctx )
               | Ok payload -> (
                   match f payload ctx with
                   | Sync (Ok _ as pipe) -> Js.Promise.resolve pipe
                   | Sync (Error _) -> Js.Promise.reject PipeError
                   | Async promise -> promise ))
        |> Js.Promise.catch (fun _ ->
               Js.Promise.resolve (Error (Response.make ~status:`badRequest ())))
        )

(** Capture a path part and since it's a matching pipe it will set the status to ok *)
let capture pipe ctx =
  match currentPathPart ctx with
  | "" -> pure ctx
  | pathPart ->
      ( pipe pathPart
      >=> setStatus (Belt.Option.getWithDefault ctx.Context.response.status `ok)
      )
        ctx

let verb verb ctx =
  if ctx.Context.request.verb <> verb then
    throwError (Response.make ~status:`notFound ())
  else pure ctx

let route pathPart ctx =
  if currentPathPart ctx = pathPart then
    pure
    @@ Context.mapMeta
         ( Meta.mapCurrentNamespace @@ fun currentNamespace ->
           currentNamespace ^ "/" ^ pathPart )
         ctx
  else throwError @@ Response.make ()

let%private respondWith body =
  (fun ctx -> pure @@ Context.mapResponse (Response.setBody body) ctx)
  >=> fun ctx ->
  setStatus (Belt.Option.getWithDefault ctx.Context.response.status `ok) ctx

let text text =
  setContentType `text
  >=> setContentLength (Js.String.length text)
  >=> respondWith @@ Serializable.fromString text

let status status =
  setStatus status
  >=> option setContentLength
        (Serializable.length @@ Serializable.fromStatus status)
  >=> respondWith @@ Serializable.fromStatus status

let json json =
  setContentType `json
  >=> option setContentLength (Serializable.length @@ Serializable.fromJson json)
  >=> respondWith @@ Serializable.fromJson json

let redirect url = setHeader "Location" url >=> setStatus `found
