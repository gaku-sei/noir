open Jest
open Expect
open! Operators
open TestHelpers
open Core

let emptyRequest = Request.make ~pathName:"" ~url:"" ~verb:`get ()

let () =
  describe "Pipe" (fun () ->
      describe "setContentType" (fun () ->
          testPipe "set content type on all kind of requests" emptyRequest
            (setStatus `notFound)
            (Response.make ~status:`notFound ())))
