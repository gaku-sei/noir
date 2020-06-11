type 'a kind =
  | Sync of 'a Context.t option
  | Async of 'a Context.t Js.Promise.t

type 'a t = 'a Context.t -> 'a kind

exception PipeError

module Infix = struct
  let ( >=> ) a b ctx =
    match a ctx with
    | Sync (Some ctx) -> b ctx
    | Sync None as none -> none
    | Async promise ->
        Async
          ( promise
          |> Js.Promise.then_ (fun ctx ->
                 match b ctx with
                 | Sync (Some ctx) -> Js.Promise.resolve ctx
                 | Sync None -> Js.Promise.reject PipeError
                 | Async promise -> promise) )

  let ( <|> ) a b ctx =
    match a ctx with
    | Sync (Some _) as ok -> ok
    | Sync None -> b ctx
    | Async promise ->
        Async
          ( promise
          |> Js.Promise.catch (fun _ ->
                 match b ctx with
                 | Sync (Some ctx) -> Js.Promise.resolve ctx
                 | Sync None -> Js.Promise.reject PipeError
                 | Async promise -> promise) )
end

open Infix

let sync ctx = Sync ctx

let syncOk ctx = Sync (Some ctx)

let async ctx = Async ctx

let ok f ctx = syncOk @@ f ctx

let%private guard f ctx = sync @@ if f ctx then Some ctx else None

let%private currentPathPart
    { Context.request = { pathName }; meta = { currentNamespace } } =
  let from = Js.String.length currentNamespace + 1 in
  let to_ = pathName |> Js.String.indexOfFrom "/" from in
  Js.String.substring pathName ~from
    ~to_:(if to_ < 0 then Js.String.length pathName else to_)

let setContentType contentType =
  ok @@ Context.mapResponse @@ Response.mapHeaders @@ Headers.set "Content-Type"
  @@ Http.ContentType.show contentType

let setContentLength contentLength =
  if contentLength < 0 then syncOk
  else
    ok @@ Context.mapResponse @@ Response.mapHeaders
    @@ Headers.set "Content-Length"
    @@ string_of_int contentLength

let setStatus status = ok @@ Context.mapResponse @@ Response.setStatus status

let setHeader key value =
  if key = "" || value = "" then syncOk
  else ok @@ Context.mapResponse @@ Response.mapHeaders @@ Headers.set key value

let setHeaders headers =
  ok @@ Context.mapResponse @@ Response.setHeaders @@ Js.Dict.fromArray
  @@ Js.Array.filter (fun (key, value) -> key <> "" && value <> "")
  @@ Js.Dict.entries headers

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
  syncOk @@ (Context.mapRequest @@ Request.setUrl url) ctx

let option pipe option =
  match option with
  | None -> fun ctx -> syncOk ctx
  | Some x -> fun ctx -> pipe x ctx

let%private handlePayload f decodedBody ctx =
  match decodedBody with
  | Ok payload -> f payload ctx
  | Error errors ->
      syncOk
      @@ Context.mapResponse (Response.setStatus `badRequest)
      @@ Context.mapResponse (Response.setBody errors) ctx

let payload decode f ({ Context.request = { body } } as ctx) =
  match body with
  | None -> setStatus `badRequest ctx
  | Some (String body) -> (handlePayload f @@ decode body) ctx
  | Some (Buffer body) ->
      ( handlePayload f @@ decode
      @@ Bindings.Node.Buffer.toStringWithEncoding body `utf8 )
        ctx
  | Some (Stream body) ->
      async
        ( Bindings.Node.Stream.Readable.consume body
        |> Js.Promise.then_ (fun body ->
               match decode body with
               | Error errors ->
                   Js.Promise.resolve
                   @@ Context.mapResponse (Response.setStatus `badRequest)
                   @@ Context.mapResponse (Response.setBody errors) ctx
               | Ok payload -> (
                   match f payload ctx with
                   | Sync (Some ctx) -> Js.Promise.resolve ctx
                   | Sync None -> Js.Promise.reject PipeError
                   | Async promise -> promise ))
        |> Js.Promise.catch (fun _ ->
               Js.Promise.resolve
               @@ Context.mapResponse (Response.setStatus `badRequest) ctx) )

(** Capture a path part and since it's a matching pipe it will set the status to ok *)
let capture pipe ctx =
  match currentPathPart ctx with
  | "" -> syncOk ctx
  | pathPart ->
      ( pipe pathPart
      >=> setStatus (Belt.Option.getWithDefault ctx.Context.response.status `ok)
      )
        ctx

let verb verb = guard (fun ctx -> ctx.request.verb = verb)

let route pathPart ctx =
  if currentPathPart ctx = pathPart then
    ( ok @@ Context.mapMeta @@ Meta.mapCurrentNamespace
    @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathPart )
      ctx
  else sync None

let%private respondWith body =
  ok @@ Context.mapResponse @@ Response.setBody body >=> fun ctx ->
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
