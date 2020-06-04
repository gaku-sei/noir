include Pipe
include Pipe.Infix

let run ~adapter pipeline =
  let module Adapter = (val adapter : Adapter.Type) in
  Adapter.listen @@ fun request ->
  pipeline @@ Context.make ~request ~response:Response.empty
  |> Js.Promise.then_ (fun { Context.response } -> Js.Promise.resolve response)
  |> Js.Promise.catch (fun _ ->
         Js.Promise.resolve @@ Response.make ~status:`notFound ())
