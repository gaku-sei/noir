open Core.Infix

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

let sync ctx = Sync ctx

let async ctx = Async ctx

let ok f ctx = sync @@ Some (f ctx)

let setContentType =
  ok <<< Context.mapResponse <<< Response.mapHeaders
  <<< Headers.set "Content-Type" <<< Http.ContentType.show

let setStatus = ok <<< Context.mapResponse <<< Response.setStatus

let setHeader key =
  ok <<< Context.mapResponse <<< Response.mapHeaders <<< Headers.set key

let setHeaders = ok <<< Context.mapResponse <<< Response.setHeaders

let text body =
  ok @@ Context.mapResponse @@ Response.setBody
  @@ Some (Serializable.fromString body)

let namespace pathName =
  ok @@ Context.mapMeta @@ Meta.mapCurrentNamespace
  @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathName

let route pathName : t =
 fun ctx ->
  if ctx.meta.currentNamespace ^ "/" ^ pathName = ctx.request.pathName then
    sync @@ Some ctx
  else sync None
