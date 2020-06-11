open Jest
open Expect
open! Operators
open ExampleTestHelpers
open Pipes

let fakeConfig = { db = "fake db" }

let fakeBaseUrl = "http://my-api.com"

(* Demonstrates how to test a "real world" application using Noir and BsJest *)

let () =
  describe "Main" (fun () ->
      describe "home" (fun () ->
          testAllPipe "listen to get requests on /" fakeConfig
            [
              ( Request.make ~verb:`get ~pathName:"/" ~url:(fakeBaseUrl ^ "/") (),
                home,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Type", "application/json");
                         ("Content-Length", "17");
                         ("X-Powered-By", "Noir");
                       ])
                  ~body:(Serializable.fromString "{\"hello\":\"world\"}")
                  () );
            ]);

      describe "hello" (fun () ->
          testAllPipe "not listen to get /hello" fakeConfig
            [
              ( Request.make ~verb:`get ~pathName:"/hello"
                  ~url:(fakeBaseUrl ^ "/hello") (),
                hello,
                Response.notFound () );
            ];

          describe "noir" (fun () ->
              testAllPipe "listen to get /hello/noir" fakeConfig
                [
                  ( Request.make ~verb:`get ~pathName:"/hello/noir"
                      ~url:(fakeBaseUrl ^ "/hello/noir")
                      (),
                    hello,
                    Response.ok
                      ~headers:
                        (Js.Dict.fromList
                           [
                             ("X-Powered-By", "Noir");
                             ("Content-Type", "text/html");
                             ("Content-Length", "19");
                           ])
                      ~body:(Serializable.fromString "Say hello to Noir!!")
                      () );
                ]);

          describe "capture" (fun () ->
              testAllPipe "listen to get /hello/{capture} and capture path part"
                fakeConfig
                [
                  ( Request.make ~verb:`get ~pathName:"/hello/foo"
                      ~url:(fakeBaseUrl ^ "/hello/foo")
                      (),
                    hello,
                    Response.ok
                      ~headers:
                        (Js.Dict.fromList
                           [
                             ("Content-Type", "text/html");
                             ("Content-Length", "16");
                           ])
                      ~body:(Serializable.fromString "Say hello to foo")
                      () );
                ]));

      describe "noContent" (fun () ->
          testAllPipe "listen get /no-content and simply return 204" fakeConfig
            [
              ( Request.make ~verb:`get ~pathName:"/no-content"
                  ~url:(fakeBaseUrl ^ "/no-content")
                  (),
                noContent,
                Response.make
                  ~headers:(Js.Dict.fromList [ ("Content-Length", "0") ])
                  ~status:`noContent
                  ~body:(Serializable.fromString "")
                  () );
            ]);

      describe "something" (fun () ->
          testAllPipe "listen get /something" fakeConfig
            [
              ( Request.make ~verb:`get ~pathName:"/something"
                  ~url:(fakeBaseUrl ^ "/something")
                  (),
                something,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "14");
                         ("Content-Type", "text/html");
                         ("Something", "Else");
                       ])
                  ~body:(Serializable.fromString "Something here")
                  () );
            ]);

      describe "withPayload" (fun () ->
          testAllPipe "serve /with-payload if payload is valid, 404 otherwise"
            fakeConfig
            [
              ( Request.make ~verb:`post ~pathName:"/with-payload"
                  ~url:(fakeBaseUrl ^ "/with-payload")
                  (),
                withPayload,
                Response.make ~status:`badRequest () );
              ( Request.make ~verb:`post ~pathName:"/with-payload"
                  ~url:(fakeBaseUrl ^ "/with-payload")
                  ~body:(Serializable.fromString "")
                  (),
                withPayload,
                Response.make ~status:`badRequest
                  ~body:(Serializable.fromString "error: Not an object")
                  () );
              ( Request.make ~verb:`post ~pathName:"/with-payload"
                  ~url:(fakeBaseUrl ^ "/with-payload")
                  ~body:
                    (Serializable.fromString "{\"age\":12,\"name\":\"foo\"}")
                  (),
                withPayload,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "30"); ("Content-Type", "text/html");
                       ])
                  ~body:
                    (Serializable.fromString "Seems that foo is 12 years old")
                  () );
            ]);

      describe "pipeline" (fun () ->
          testAllPipe "same tests in the pipeline context" fakeConfig
            [
              ( Request.make ~verb:`get ~pathName:"/" ~url:(fakeBaseUrl ^ "/") (),
                home,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Type", "application/json");
                         ("Content-Length", "17");
                         ("X-Powered-By", "Noir");
                       ])
                  ~body:(Serializable.fromString "{\"hello\":\"world\"}")
                  () );
              ( Request.make ~verb:`get ~pathName:"/hello"
                  ~url:(fakeBaseUrl ^ "/hello") (),
                hello,
                Response.notFound () );
              ( Request.make ~verb:`get ~pathName:"/hello/noir"
                  ~url:(fakeBaseUrl ^ "/hello/noir")
                  (),
                hello,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("X-Powered-By", "Noir");
                         ("Content-Type", "text/html");
                         ("Content-Length", "19");
                       ])
                  ~body:(Serializable.fromString "Say hello to Noir!!")
                  () );
              ( Request.make ~verb:`get ~pathName:"/hello/foo"
                  ~url:(fakeBaseUrl ^ "/hello/foo")
                  (),
                hello,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Type", "text/html"); ("Content-Length", "16");
                       ])
                  ~body:(Serializable.fromString "Say hello to foo")
                  () );
              ( Request.make ~verb:`get ~pathName:"/no-content"
                  ~url:(fakeBaseUrl ^ "/no-content")
                  (),
                noContent,
                Response.make
                  ~headers:(Js.Dict.fromList [ ("Content-Length", "0") ])
                  ~status:`noContent
                  ~body:(Serializable.fromString "")
                  () );
              ( Request.make ~verb:`get ~pathName:"/something"
                  ~url:(fakeBaseUrl ^ "/something")
                  (),
                something,
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "14");
                         ("Content-Type", "text/html");
                         ("Something", "Else");
                       ])
                  ~body:(Serializable.fromString "Something here")
                  () );
            ]))
