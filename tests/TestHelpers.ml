open Jest
open Expect
open! Operators

let testPipe description request pipeline response =
  testPromise description (fun () ->
      TestAdapter.run ~config:() ~request ~pipeline
      |> Js.Promise.then_ (fun response' ->
             Js.Promise.resolve (expect response' = response)))

let testAllPipe description tests =
  ignore
  @@ Belt.List.mapWithIndex tests (fun index (request, pipeline, response) ->
         testPromise
           (description ^ " - " ^ Belt.Int.toString (index + 1))
           (fun () ->
             TestAdapter.run ~config:() ~request ~pipeline
             |> Js.Promise.then_ (fun response' ->
                    Js.Promise.resolve (expect response' = response))))

module Skip = struct
  let testPipe description _request _pipeline _response =
    Skip.test description @@ fun _ -> expect true = false

  let testAllPipe description _tests =
    Skip.test description @@ fun _ -> expect true = false
end
