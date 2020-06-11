exception PromiseException of Js.Promise.error

(** Will create a valid adapter for testing purposes *)
let make ?onResolve ?onReject request =
  ( module struct
    let listen f =
      f request
      |> Js.Promise.then_ (fun response ->
             Belt.Option.forEach onResolve (fun onResolve -> onResolve response);
             Js.Promise.resolve response)
      |> Js.Promise.catch (fun error ->
             Belt.Option.forEach onReject (fun onReject -> onReject error);
             Js.Promise.reject @@ PromiseException error)
      |> ignore
  end : Adapter.Type )

(** Fake run like function that takes a request and a pipeline and returns a promise of response *)
let run ~config ~request ~pipeline =
  Js.Promise.make (fun ~resolve ~reject ->
      Core.run
        ~adapter:
          (make
             ~onResolve:(fun response -> (resolve response [@bs]))
             ~onReject:(fun error -> (reject (PromiseException error) [@bs]))
             request)
        ~config pipeline)
