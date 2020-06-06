open Jest
open Expect
open! Operators
open TestHelpers
open Core

let emptyRequest = Request.make ~pathName:"" ~url:"" ~verb:`get ()

let () =
  describe "Core" (fun () ->
      describe "run" (fun () ->
          testAllPipe "in progress"
            [ (Request.empty, syncOk, Response.notFound ()) ]))
