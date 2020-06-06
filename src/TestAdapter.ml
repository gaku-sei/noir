let make ?onResolve ?onReject request =
  ( module struct
    exception PromiseError of Js.Promise.error

    let listen f =
      f request
      |> Js.Promise.then_ (fun response ->
             Belt.Option.forEach onResolve (fun onResolve -> onResolve response);
             Js.Promise.resolve response)
      |> Js.Promise.catch (fun error ->
             Belt.Option.forEach onReject (fun onReject -> onReject error);
             Js.Promise.reject @@ PromiseError error)
      |> ignore
  end : Adapter.Type )
