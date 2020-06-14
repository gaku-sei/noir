include Pipe
include CommonPipe

let run ~adapter ~config pipeline =
  let module Adapter = (val adapter : Adapter.Type) in
  Adapter.listen @@ fun request ->
  match
    pipeline @@ Context.make ~config ~request ~response:(Response.make ())
  with
  | Sync (Ok { Context.response = { status = None } as response }) ->
      Js.Promise.resolve @@ Response.setStatus `notFound response
  | Sync (Ok { Context.response }) -> Js.Promise.resolve response
  | Sync (Error _response) -> Js.Promise.resolve @@ Response.notFound ()
  | Async promise ->
      promise
      |> Js.Promise.then_ (function
           | Ok { Context.response = { status = None } as response } ->
               Js.Promise.resolve @@ Response.setStatus `notFound response
           | Ok { Context.response } -> Js.Promise.resolve response
           | Error _response -> Js.Promise.reject PipeError)
      |> Js.Promise.catch (fun _ -> Js.Promise.resolve @@ Response.notFound ())
