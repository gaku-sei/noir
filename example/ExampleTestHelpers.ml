open Jest
open Expect
open! Operators

(* A simple test helpers that uses the TestAdapter under the hood *)

let testAllPipe description config tests =
  ignore
  @@ Belt.List.mapWithIndex tests (fun index (request, pipeline, response) ->
         testPromise
           (description ^ " - " ^ string_of_int (index + 1))
           (fun () ->
             TestAdapter.run ~config ~request ~pipeline
             |> Js.Promise.then_ (fun response' ->
                    Js.Promise.resolve (expect response' = response))))
