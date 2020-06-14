type 'a t =
  | Sync of ('a, Response.t) result
  | Async of ('a, Response.t) result Js.Promise.t

exception PipeError

(* Functor *)

let map : 'a 'b. ('a -> 'b) -> 'a t -> 'b t =
 fun f -> function
  | Sync (Ok ctx) -> Sync (Ok (f ctx))
  | Sync (Error error) -> Sync (Error error)
  | Async _ -> Obj.magic ()

let ( <$> ) = map

let voidRight ctx = map (fun _ -> ctx)

let ( <$ ) = voidRight

let voidLeft pipe ctx = map (fun _ -> ctx) pipe

let ( $> ) = voidLeft

let flap pipe ctx = map (fun f -> f ctx) pipe

let ( <@> ) = flap

(* Apply *)

let apply : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
 fun pipe1 pipe2 ->
  match (pipe1, pipe2) with
  | Sync (Ok f), Sync (Ok ctx) -> Sync (Ok (f ctx))
  | _ -> Obj.magic ()

let ( <*> ) = apply

let applyFirst a b = (fun x _ -> x) <$> a <*> b

let ( <* ) = applyFirst

let applySecond a b = (fun _ x -> x) <$> a <*> b

let ( *> ) = applySecond

(* Applicative *)

let pure : 'a. 'a -> 'a t = fun value -> Sync (Ok value)

(* Monad *)

let flatMap : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
 fun pipe f ->
  match pipe with
  | Sync (Ok ctx) -> f ctx
  | Sync (Error error) -> Sync (Error error)
  | Async _ -> Obj.magic ()

let ( >>= ) = flatMap

let flatMapFlipped : 'a 'b. ('a -> 'b t) -> 'a t -> 'b t =
 fun f -> function
  | Sync (Ok ctx) -> f ctx
  | Sync (Error error) -> Sync (Error error)
  | Async _ -> Obj.magic ()

let ( =<< ) = flatMapFlipped

let composeKleisli : 'a 'c 'd. ('d -> 'c t) -> ('c -> 'a t) -> 'd -> 'a t =
 fun pipe1 pipe2 value -> pipe1 value >>= pipe2

let ( >=> ) = composeKleisli

let composeKleisliFlipped pipe1 pipe2 value = pipe1 =<< pipe2 value

let ( <=< ) = composeKleisliFlipped

let join pipe = pipe >>= fun value -> value

(* MonadThrow *)

let throwError : 'a. Response.t -> 'a t = fun error -> Sync (Error error)

(* MonadError *)

let catchError : 'a. 'a t -> (Response.t -> 'a t) -> 'a t =
 fun pipe f ->
  match pipe with
  | Sync (Error error) -> f error
  | Sync (Ok _) as ok -> ok
  | Async _ -> Obj.magic ()

(* Extend *)

let extend : 'a 'b. ('a t -> 'b) -> 'a t -> 'b t = fun f pipe -> pure @@ f pipe

let ( <<= ) = extend

let extendFlipped pipe f = pure @@ f pipe

let ( =>> ) = extendFlipped

let duplicate pipe = extend (fun x -> x) pipe

(* Alt *)

let alt : 'a. 'a t -> 'a t -> 'a t =
 fun pipe1 pipe2 ->
  match pipe1 with
  | Sync (Error _) -> pipe2
  | Sync (Ok _) -> pipe1
  | Async _ -> Obj.magic ()

let ( <|> ) = alt
