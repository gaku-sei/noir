open Util.Infix

type kind = Sync of Context.t option | Async of Context.t Js.Promise.t

type t = Context.t -> kind

exception PipeError

module Infix = struct
  (* Kleisli composition *)
  let ( >=> ) (a : t) (b : t) : t =
   fun ctx ->
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

  (* Alt operator *)
  let ( <|> ) (a : t) (b : t) : t =
   fun ctx ->
    match a ctx with
    | Sync (Some ctx) -> Sync (Some ctx)
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

let async ctx = Async ctx

let ok f ctx = sync @@ Some (f ctx)

let%private respondWith = ok <<< Context.mapResponse <<< Response.setBody

let%private guard f : t = fun ctx -> sync @@ if f ctx then Some ctx else None

let%private currentPathPart
    ({ request = { pathName }; meta = { currentNamespace } } : Context.t) =
  let from = Js.String.length currentNamespace + 1 in
  let to_ = pathName |> Js.String.indexOfFrom "/" from in
  Js.String.substring pathName ~from
    ~to_:(if to_ < 0 then Js.String.length pathName else to_)

let setContentType =
  ok <<< Context.mapResponse <<< Response.mapHeaders
  <<< Headers.set "Content-Type" <<< Http.ContentType.show

let setContentLength =
  ok <<< Context.mapResponse <<< Response.mapHeaders
  <<< Headers.set "Content-Length"

let setStatus = ok <<< Context.mapResponse <<< Response.setStatus

let setHeader key =
  ok <<< Context.mapResponse <<< Response.mapHeaders <<< Headers.set key

let setHeaders = ok <<< Context.mapResponse <<< Response.setHeaders

let option pipe option : t =
  match option with
  | None -> fun ctx -> sync @@ Some ctx
  | Some x -> fun ctx -> pipe x ctx

let text text : t =
  setContentType `text
  >=> setContentLength (string_of_int @@ Js.String.length text)
  >=> respondWith @@ Serializable.fromString text

let status status : t =
  setStatus status
  >=> option
        (setContentLength <<< string_of_int)
        (Serializable.length @@ Serializable.fromStatus status)
  >=> respondWith @@ Serializable.fromStatus status

let json json : t =
  setContentType `json
  >=> option
        (setContentLength <<< string_of_int)
        (Serializable.length @@ Serializable.fromJson json)
  >=> respondWith @@ Serializable.fromJson json

let namespace pathName : t =
 fun ctx ->
  if currentPathPart ctx = pathName then
    ( ok @@ Context.mapMeta @@ Meta.mapCurrentNamespace
    @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathName )
      ctx
  else sync None

let%private handlePayload f decodedBody : t =
 fun ctx ->
  match decodedBody with
  | Ok payload -> (
      match f payload ctx with
      | Sync (Some ctx) -> sync @@ Some ctx
      | Sync None -> sync None
      | Async promise -> async promise )
  | Error errors ->
      sync
      @@ Some
           ( Context.mapResponse (Response.setStatus `badRequest)
           @@ Context.mapResponse (Response.setBody errors) ctx )

let payload decode f : t =
 fun ({ request = { body } } as ctx) ->
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

let capture f : t = fun ctx -> f (currentPathPart ctx) ctx

let verb verb : t = guard (fun ctx -> ctx.request.verb = verb)

let route pathName : t =
  guard (fun ctx ->
      ctx.meta.currentNamespace ^ "/" ^ pathName = ctx.request.pathName)
