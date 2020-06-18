include Pipe

let run ~adapter ~config pipeline =
  let module Adapter = (val adapter : Adapter.Type) in
  Adapter.listen @@ fun request ->
  match
    pipeline @@ Context.make ~config ~request ~response:(Response.make ())
  with
  | Sync (Finish response) -> Js.Promise.resolve response
  | Sync (Pass _) -> Js.Promise.resolve @@ Response.notFound ()
  | Sync (Continue { response = { status = None } as response }) ->
      Js.Promise.resolve @@ Response.setStatus `notFound response
  | Sync (Continue { response }) -> Js.Promise.resolve response
  | Async promise ->
      promise
      |> Js.Promise.then_ (function
           | Finish response -> Js.Promise.resolve response
           | Pass _ -> Js.Promise.resolve @@ Response.notFound ()
           | Continue { response = { status = None } as response } ->
               Js.Promise.resolve @@ Response.setStatus `notFound response
           | Continue { response } -> Js.Promise.resolve response)
      |> Js.Promise.catch (fun error ->
             Js.Promise.resolve
             @@ Response.make ~status:`internalServerError
                  ~body:(Serializable.fromString @@ Js.String.make error)
                  ())
