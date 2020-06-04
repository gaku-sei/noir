open Core.Infix

type t = Context.t -> Context.t Js.Promise.t

module Infix = struct
  (* Kleisli composition *)
  let ( >=> ) (a : t) (b : t) (ctx : Context.t) = a ctx |> Js.Promise.then_ b

  (* Alt operator *)
  let ( <|> ) (a : t) (b : t) (ctx : Context.t) =
    a ctx |> Js.Promise.catch (fun _ -> b ctx)
end

let setContentType =
  Context.ok <<< Context.mapResponse <<< Response.mapHeaders
  <<< Headers.set "Content-Type" <<< Http.ContentType.show

let setStatus = Context.ok <<< Context.mapResponse <<< Response.setStatus

let setHeader key =
  Context.ok <<< Context.mapResponse <<< Response.mapHeaders <<< Headers.set key

let setHeaders = Context.ok <<< Context.mapResponse <<< Response.setHeaders

let text body =
  Context.ok @@ Context.mapResponse @@ Response.setBody
  @@ Some (Serializable.fromString body)

let namespace pathName =
  Context.ok @@ Context.mapMeta @@ Meta.mapCurrentNamespace
  @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathName

let route pathName : t =
 fun ctx ->
  if ctx.meta.currentNamespace ^ "/" ^ pathName = ctx.request.pathName then
    Js.Promise.resolve ctx
  else Js.Promise.reject Exceptions.RouteNotMatched
