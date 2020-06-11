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
           (description ^ " - " ^ string_of_int (index + 1))
           (fun () ->
             TestAdapter.run ~config:() ~request ~pipeline
             |> Js.Promise.then_ (fun response' ->
                    Js.Promise.resolve (expect response' = response))))
