open Core
open Pipes

let () =
  run ~adapter:(module NativeAdapter) ~config:{ db = "postgres" } pipeline
