open Jest
open Expect
open! Operators

// A simple test helpers that uses the TestAdapter under the hood

let testAllPipe = (description, config, tests) =>
  ignore(
    tests->Js.Array2.mapi(((request, pipeline, response), index) =>
      testPromise(`${description} - ${Belt.Int.toString(index + 1)}`, () =>
        TestAdapter.run(~config, ~request, ~pipeline)->Js.Promise.then_(
          response' => Js.Promise.resolve(expect(response') == response),
          _,
        )
      )
    ),
  )
