open Jest
open Expect
open! Operators
open! TestHelpers
open Core

let () =
  describe "Pipe" (fun () ->
      describe "setContentType" (fun () ->
          testAllPipe "set content type on all kind of requests"
            [
              ( Request.empty,
                setContentType `json,
                Response.notFound
                  ~headers:
                    (Js.Dict.fromList [ ("Content-Type", "application/json") ])
                  () );
              ( Request.empty,
                setContentType `text,
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("Content-Type", "text/html") ])
                  () );
            ]);

      describe "setContentLength" (fun () ->
          testAllPipe "set content length on all kind of requests"
            [
              ( Request.empty,
                setContentLength 200,
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("Content-Length", "200") ])
                  () );
              ( Request.empty,
                setContentLength 0,
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("Content-Length", "0") ])
                  () );
              (Request.empty, setContentLength (-10), Response.notFound ());
            ]);

      describe "setStatus" (fun () ->
          testAllPipe "set status on all kind of requests"
            [
              (Request.empty, setStatus `notFound, Response.notFound ());
              (Request.empty, setStatus `notFound, Response.notFound ());
              (Request.empty, setStatus `ok, Response.ok ());
              ( Request.empty,
                setStatus `unauthorized,
                Response.make ~status:`unauthorized () );
            ]);

      describe "setHeader" (fun () ->
          testAllPipe "set header on all kind of requests"
            [
              ( Request.empty,
                setHeader "X-Whatever" "whatever",
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("X-Whatever", "whatever") ])
                  () );
              (Request.empty, setHeader "" "", Response.notFound ());
            ]);

      describe "setHeaders" (fun () ->
          testAllPipe "set headers on all kind of requests"
            [
              ( Request.empty,
                setHeaders @@ Js.Dict.fromList [ ("X-Whatever", "whatever") ],
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("X-Whatever", "whatever") ])
                  () );
              ( Request.empty,
                setHeaders
                @@ Js.Dict.fromList [ ("X-Whatever", "whatever"); ("", "") ],
                Response.notFound
                  ~headers:(Js.Dict.fromList [ ("X-Whatever", "whatever") ])
                  () );
              ( Request.empty,
                setHeaders @@ Js.Dict.fromList [ ("X-Whatever", "") ],
                Response.notFound () );
              ( Request.empty,
                setHeaders @@ Js.Dict.fromList [ ("", "whatever") ],
                Response.notFound () );
              ( Request.empty,
                setHeaders @@ Js.Dict.fromList [ ("", "") ],
                Response.notFound () );
              ( Request.empty,
                setHeaders @@ Js.Dict.empty (),
                Response.notFound () );
            ]);

      describe "setCookie" (fun () ->
          testAllPipe "set cookie on all kind of requests"
            [
              ( Request.empty,
                setCookie "foo" "bar" >=> text "cookie set",
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Set-Cookie", "foo=bar");
                         ("Content-Length", "10");
                         ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "cookie set")
                  () );
              ( Request.empty,
                setCookie
                  ~expires:(Js.Date.makeWithYM ~year:2020. ~month:0. ())
                  ~maxAge:1 ~domain:"foobar" ~path:"/" ~secure:true
                  ~httpOnly:true ~sameSite:`strict "foo" "bar"
                >=> text "cookie set",
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ( "Set-Cookie",
                           "foo=bar; Expires=Tue, 31 Dec 2019 15:00:00 GMT; \
                            Max-Age=1; Domain=foobar; Path=/; Secure; \
                            HttpOnly; SameSite=Strict" );
                         ("Content-Length", "10");
                         ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "cookie set")
                  () );
            ]);

      describe "resolveUrl" (fun () ->
          testAllPipe "resolve url and set the ctx accordingly"
            [
              ( Request.empty,
                ( resolveUrl ~secure:false >=> fun ctx ->
                  ( text
                  @@ Belt.Option.getWithDefault ctx.request.url "incorrect url"
                  )
                    ctx ),
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "13"); ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "incorrect url")
                  () );
              ( Request.make ~pathName:"/foo" ~verb:`get (),
                ( resolveUrl ~secure:false >=> fun ctx ->
                  ( text
                  @@ Belt.Option.getWithDefault ctx.request.url "incorrect url"
                  )
                    ctx ),
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "13"); ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "incorrect url")
                  () );
              ( Request.make ~pathName:"/foo"
                  ~headers:(Js.Dict.fromList [ ("host", "foobar.com") ])
                  ~verb:`get (),
                ( resolveUrl ~secure:false >=> fun ctx ->
                  ( text
                  @@ Belt.Option.getWithDefault ctx.request.url "incorrect url"
                  )
                    ctx ),
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "21"); ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "http://foobar.com/foo")
                  () );
              ( Request.make ~pathName:"/foo"
                  ~headers:(Js.Dict.fromList [ ("host", "foobar.com") ])
                  ~verb:`get (),
                ( resolveUrl ~secure:true >=> fun ctx ->
                  ( text
                  @@ Belt.Option.getWithDefault ctx.request.url "incorrect url"
                  )
                    ctx ),
                Response.ok
                  ~headers:
                    (Js.Dict.fromList
                       [
                         ("Content-Length", "22"); ("Content-Type", "text/html");
                       ])
                  ~body:(Serializable.fromString "https://foobar.com/foo")
                  () );
            ]);

      describe "option" (fun () ->
          testAllPipe "execute pipe only if argument is some thing"
            [
              (Request.empty, option setStatus None, Response.notFound ());
              ( Request.empty,
                option setStatus @@ Some `noContent,
                Response.make ~status:`noContent () );
            ]);

      describe "json" (fun () ->
          Skip.testAllPipe
            "set the body, the content type, and the content length accordingly"
            []);

      describe "text" (fun () ->
          Skip.testAllPipe
            "set the body, the content type, and the content length accordingly"
            []);

      describe "status" (fun () ->
          Skip.testAllPipe
            "set the status code, the body, and the content length accordingly"
            []);

      describe "redirect" (fun () ->
          testAllPipe "set location header accordingly"
            [
              ( Request.empty,
                redirect "/foobar",
                Response.make ~status:`found
                  ~headers:(Js.Dict.fromList [ ("Location", "/foobar") ])
                  () );
            ]);

      describe "payload" (fun () ->
          Skip.testAllPipe
            "return a 400 when decoder fails, and provide the decoded payload \
             if decoder passes"
            []);

      describe "capture" (fun () ->
          Skip.testAllPipe "capture the current part of the url path" []);

      describe "verb" (fun () ->
          Skip.testAllPipe
            "filter out requests which verb don't match expected verb" []);

      describe "route" (fun () ->
          Skip.testAllPipe
            "continue if current path part match, and there is no other path \
             part, shouldn't match otherwise"
            []);

      describe "Infix" (fun () ->
          describe "compose >=>" (fun () ->
              Skip.testAllPipe
                "handle complex compositions of pipes (pipelines), notably \
                 namespaces and routes"
                []);

          describe "alt <|>" (fun () ->
              Skip.testAllPipe
                "handle complex alternatives of pipes (pipelines), notably \
                 namespaces and routes"
                [])))
