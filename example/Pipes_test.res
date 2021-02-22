open Jest
open Expect
open! Operators
open ExampleTestHelpers
open Pipes

let fakeConfig = {db: "fake db"}

// Demonstrates how to test a "real world" application using Noir and BsJest

let () = describe("Main", () => {
  describe("home", () =>
    testAllPipe(
      "listen to get requests on /",
      fakeConfig,
      [
        (
          Request.make(~verb=#get, ~pathName="/", ()),
          home,
          Response.ok(
            ~headers=Js.Dict.fromArray([
              ("Content-Type", "application/json"),
              ("Content-Length", "17"),
              ("X-Powered-By", "Noir"),
            ]),
            ~body=Serializable.fromString("{\"hello\":\"world\"}"),
            (),
          ),
        ),
      ],
    )
  )

  describe("hello", () => {
    testAllPipe(
      "not listen to get /hello",
      fakeConfig,
      [(Request.make(~verb=#get, ~pathName="/hello", ()), hello, Response.notFound())],
    )

    describe("noir", () =>
      testAllPipe(
        "listen to get /hello/noir",
        fakeConfig,
        [
          (
            Request.make(~verb=#get, ~pathName="/hello/noir", ()),
            hello,
            Response.ok(
              ~headers=Js.Dict.fromArray([
                ("X-Powered-By", "Noir"),
                ("Content-Type", "text/html"),
                ("Content-Length", "19"),
              ]),
              ~body=Serializable.fromString("Say hello to Noir!!"),
              (),
            ),
          ),
        ],
      )
    )

    describe("capture", () =>
      testAllPipe(
        "listen to get /hello/{capture} and capture path part",
        fakeConfig,
        [
          (
            Request.make(~verb=#get, ~pathName="/hello/foo", ()),
            hello,
            Response.ok(
              ~headers=Js.Dict.fromArray([("Content-Type", "text/html"), ("Content-Length", "16")]),
              ~body=Serializable.fromString("Say hello to foo"),
              (),
            ),
          ),
        ],
      )
    )
  })

  describe("noContent", () =>
    testAllPipe(
      "listen get /no-content and simply return 204",
      fakeConfig,
      [
        (
          Request.make(~verb=#get, ~pathName="/no-content", ()),
          noContent,
          Response.make(
            ~headers=Js.Dict.fromArray([("Content-Length", "0")]),
            ~status=#noContent,
            ~body=Serializable.fromString(""),
            (),
          ),
        ),
      ],
    )
  )

  describe("something", () =>
    testAllPipe(
      "listen get /something",
      fakeConfig,
      [
        (
          Request.make(~verb=#get, ~pathName="/something", ()),
          something,
          Response.ok(
            ~headers=Js.Dict.fromArray([
              ("Content-Length", "14"),
              ("Content-Type", "text/html"),
              ("Something", "Else"),
            ]),
            ~body=Serializable.fromString("Something here"),
            (),
          ),
        ),
      ],
    )
  )

  describe("withPayload", () =>
    testAllPipe(
      "serve /with-payload if payload is valid, 404 otherwise",
      fakeConfig,
      [
        (
          Request.make(~verb=#post, ~pathName="/with-payload", ()),
          withPayload,
          Response.make(~status=#badRequest, ()),
        ),
        (
          Request.make(
            ~verb=#post,
            ~pathName="/with-payload",
            ~body=Serializable.fromString(""),
            (),
          ),
          withPayload,
          Response.make(
            ~status=#badRequest,
            ~body=Serializable.fromString("error: Not an object"),
            (),
          ),
        ),
        (
          Request.make(
            ~verb=#post,
            ~pathName="/with-payload",
            ~body=Serializable.fromString("{\"age\":12,\"name\":\"foo\"}"),
            (),
          ),
          withPayload,
          Response.ok(
            ~headers=Js.Dict.fromArray([("Content-Length", "33"), ("Content-Type", "text/html")]),
            ~body=Serializable.fromString("It seems that foo is 12 years old"),
            (),
          ),
        ),
      ],
    )
  )

  describe("pipeline", () =>
    testAllPipe(
      "same tests in the pipeline context",
      fakeConfig,
      [
        (
          Request.make(~verb=#get, ~pathName="/", ()),
          home,
          Response.ok(
            ~headers=Js.Dict.fromArray([
              ("Content-Type", "application/json"),
              ("Content-Length", "17"),
              ("X-Powered-By", "Noir"),
            ]),
            ~body=Serializable.fromString("{\"hello\":\"world\"}"),
            (),
          ),
        ),
        (Request.make(~verb=#get, ~pathName="/hello", ()), hello, Response.notFound()),
        (
          Request.make(~verb=#get, ~pathName="/hello/noir", ()),
          hello,
          Response.ok(
            ~headers=Js.Dict.fromArray([
              ("X-Powered-By", "Noir"),
              ("Content-Type", "text/html"),
              ("Content-Length", "19"),
            ]),
            ~body=Serializable.fromString("Say hello to Noir!!"),
            (),
          ),
        ),
        (
          Request.make(~verb=#get, ~pathName="/hello/foo", ()),
          hello,
          Response.ok(
            ~headers=Js.Dict.fromArray([("Content-Type", "text/html"), ("Content-Length", "16")]),
            ~body=Serializable.fromString("Say hello to foo"),
            (),
          ),
        ),
        (
          Request.make(~verb=#get, ~pathName="/no-content", ()),
          noContent,
          Response.make(
            ~headers=Js.Dict.fromArray([("Content-Length", "0")]),
            ~status=#noContent,
            ~body=Serializable.fromString(""),
            (),
          ),
        ),
        (
          Request.make(~verb=#get, ~pathName="/something", ()),
          something,
          Response.ok(
            ~headers=Js.Dict.fromArray([
              ("Content-Length", "14"),
              ("Content-Type", "text/html"),
              ("Something", "Else"),
            ]),
            ~body=Serializable.fromString("Something here"),
            (),
          ),
        ),
      ],
    )
  )
})
