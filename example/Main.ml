open Core
open Pipes

let () =
  run
    ~adapter:
      (NativeAdapter.make
         ~onListen:(fun () ->
           Js.log "Noir example started: http://localhost:3000")
         ())
    ~config:{ db = "postgres" } pipeline
