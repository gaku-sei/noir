include Pipe
include Pipe.Infix

let run ~adapter ~config pipeline =
  let module Adapter = (val adapter : Adapter.Type) in
  Adapter.listen @@ fun request ->
  match
    pipeline @@ Context.make ~config ~request ~response:(Response.make ())
  with
  | Sync (Some { Context.response = { status = None } as response }) ->
      Js.Promise.resolve @@ Response.setStatus `notFound response
  | Sync (Some { Context.response }) -> Js.Promise.resolve response
  | Sync None -> Js.Promise.resolve @@ Response.notFound ()
  | Async promise ->
      promise
      |> Js.Promise.then_ (function
           | { Context.response = { status = None } as response } ->
               Js.Promise.resolve @@ Response.setStatus `notFound response
           | { Context.response } -> Js.Promise.resolve response)
      |> Js.Promise.catch (fun _ -> Js.Promise.resolve @@ Response.notFound ())
