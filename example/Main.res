open Core
open Pipes

run(
  ~adapter=NativeAdapter.make(
    ~onListen=() => Js.log("Noir example started: http://localhost:3000"),
    (),
  ),
  ~config={db: "postgres"},
  ~pipeline,
)
