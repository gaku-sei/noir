open Jest
open Expect
open! Operators
open TestHelpers
open Core

let () =
  describe "Serializable" (fun () ->
      describe "fromStatus" (fun () ->
          testAllPipe "in progress"
            [ (Request.empty, syncOk, Response.notFound ()) ]);

      describe "fromJson" (fun () -> testAllPipe "in progress" []);

      describe "length" (fun () -> testAllPipe "in progress" []))
