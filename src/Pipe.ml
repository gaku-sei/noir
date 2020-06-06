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

let setStatus = ok <<< Context.mapResponse <<< Response.setStatus

let setHeader key =
  ok <<< Context.mapResponse <<< Response.mapHeaders <<< Headers.set key

let setHeaders = ok <<< Context.mapResponse <<< Response.setHeaders

let text text =
  setContentType `text >=> respondWith @@ Serializable.fromString text

let status status =
  setStatus status >=> respondWith @@ Serializable.fromStatus status

let json json =
  setContentType `json >=> respondWith @@ Serializable.fromJson json

let namespace pathName ctx =
  if currentPathPart ctx = pathName then
    ( ok @@ Context.mapMeta @@ Meta.mapCurrentNamespace
    @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathName )
      ctx
  else sync None

let capture f : t = fun ctx -> f (currentPathPart ctx) ctx

let verb verb : t = guard (fun ctx -> ctx.request.verb = verb)

let route pathName : t =
  guard (fun ctx ->
      ctx.meta.currentNamespace ^ "/" ^ pathName = ctx.request.pathName)
