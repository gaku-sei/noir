# NOIR

_Functional and abstract api builder_

## Features

- _Functional_, Noir is written in OCaml and it allows you to write your application in ReasonML or OCaml thanks to the [BuckleScript](https://bucklescript.github.io/) compiler
- _Abstract_, Noir doesn't enforce any web server, neither [Express](https://expressjs.com/), or [Fastify](https://expressjs.com/), instead you can implement your own _adapter_ for any library and framework
- _Noir_ is fast, and comes with a default _adapter_ using the standard http and https modules.

## Concepts

### Pipes

All request handlers are built using _pipes_. A pipe is a simple function that takes a `Context`, containing the request, and the response, and returns a Promise of context, that's it.

```reason
let myPipe = ctx =>
  asyncTask
  |> Js.Promise.then(_ => {
    let newCtx = doSomethingWithTheCtx(ctx);

    Js.Promise.resolve(newCtx);
  });
```

Not only are pipes easy to define, but they are also easy to combine!

```reason
let home =
  route("")
  >=> setContentType(`json)
  >=> setHeader("My", "Header")
  >=> text("All good");
```

### Routes

Well, technically not a new concept since routes are "just" pipes... Routing is achieved using the alt operator `<|>` that will allow Noir to pick the route that matches the request!

```reason
homeRoute <|> anOtherRoute <|> etc
```

And that's about it. As said in the Pipes section, a pipe returns a `Promise`, if this promise is rejected, the pipe is not used, and the next route is used instead.

You can also use the `namespace` pipe to group routes together:

```reason
namespace "foo" >=> (
  (route "bar" >=> "I match /foo/bar")
  <|>
  (route "qux" >=> "I match /foo/qux")
);
```

## Example

```reason
let home =
  route("")
  >=> setContentType(`json)
  >=> setHeader("My", "Header")
  >=> text("All good");

let hello =
  route("hello")
  >=> text("Hello");

let pipeline = home <|> hello;

run(~adapter=(module NativeAdapter), pipeline);
```
