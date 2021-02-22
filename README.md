# NOIR

_Functional and abstract api builder_

## Features

- _Functional_, Noir is written in OCaml and it allows you to write your application in ReasonML or OCaml thanks to the [ReScript](https://rescript-lang.org//) compiler
- _Abstract_, Noir doesn't enforce any web server, neither [Express](https://expressjs.com/), or [Fastify](https://expressjs.com/), instead you can implement your own _adapter_ for any library and framework
- _Noir_ is fast, and comes with a default _adapter_ using the standard http and https modules.

## Concepts

### Pipes

All request handlers are built using _pipes_. A pipe is a simple function that takes a `Context`, containing the request, and the response, and returns a Promise of context, that's it.

ReScript:

```rescript
let myPipe = ctx => asyncTask->Js.Promise.then(_ => {
    let newCtx = doSomethingWithTheCtx(ctx)

    Js.Promise.resolve(newCtx)
  }, _)
```

OCaml:

```ocaml
let myPipe ctx =
  asyncTask
  |> Js.Promise.then_ (fun _ ->
         let newCtx = doSomethingWithTheCtx ctx in

         Js.Promise.resolvenewCtx)
```

Not only are pipes easy to define, but they are also easy to combine!

ReScript:

```rescript
let home =
  route("")
  ->pipe(setContentType(#json))
  ->pipe(setHeader("My", "Header"))
  ->pipe(text("All good"))
```

OCaml:

```ocaml
let home =
  route ""
  >=> setContentType `json
  >=> setHeader "My" "Header"
  >=> text "All good"
```

### Routes

Technically not a new concept since routes are pipes under the hood. Routing is achieved using the `alt` function (or the `<|>` operator on OCaml) that will allow Noir to pick the route that matches the request!

```rescript
homeRoute->alt(anOtherRoute)->alt(etc)
```

```ocaml
homeRoute <|> anOtherRoute <|> etc
```

You can also use the `oneOf` function:

```rescript
oneOf([homeRoute, anOtherRoute, etc])
```

```ocaml
oneOf [|homeRoute; anOtherRoute; etc|]
```

As said in the Pipes section, a pipe returns a `Promise`, if this promise is rejected, the pipe is not used, and the next route is used instead.

You can also use the `namespace` pipe to group routes together:

```rescript
namespace("foo")->pipe(
  oneOf([
    route("bar")->pipe(text("I match /foo/bar")),
    route("qux")->pipe(text("I match /foo/qux"))
  ]),
)
```

```ocaml
namespace "foo" >=> (
  (route "bar" >=> text "I match /foo/bar") <|>
  (route "qux" >=> text "I match /foo/qux")
)
```

## Example

```rescript
let home =
  route("")
  ->pipe(setContentType(#json))
  ->pipe(setHeader("My", "Header"))
  ->pipe(text("All good"))

let hello =
  route("hello")
  ->pipe(text("Hello"))

let pipeline = oneOf([home, hello])

run(~adapter=NativeAdapter.make(), ~config=(), ~pipeline)
```

```ocaml
let home =
  route ""
  >=> setContentType `json
  >=> setHeader "My" "Header"
  >=> text "All good"

let hello =
  route "hello"
  >=> text "Hello"

let pipeline = home <|> hello

let () = run ~adapter:(NativeAdapter.make ()) ~config:() ~pipeline
```

Noir has much to offer. It supports Promises and Node Streams, allows for both synchronous and asynchronous pipes, lets you bring your own adapter to interface with Express, Polka, Fastify, etc... Adapters are pretty simple to implement and should be modules that contain a single `listen` function:

```rescript
let listen: (Request.t => Js.Promise.t<Response.t>) => unit
```

Where `Request` and `Response` are both simple objects defined in this package.

## Tests

Tests can be ran using the `yarn test` command, and code coverage can be generated and served using `yarn test:coverage`. The coverage will be served on [http://localhost:5000](http://localhost:5000).
