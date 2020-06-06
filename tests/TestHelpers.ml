open Jest
open Expect
open! Operators

let testPipe description request pipeline response =
  testPromise description (fun () ->
      TestAdapter.run request pipeline
      |> Js.Promise.then_ (fun response' ->
             Js.Promise.resolve (expect response' = response)))
