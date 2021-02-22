(**
  Continue: The pipe is "valid", and should go on
  Pass: The pipe's expectations are not met (wrong path, verb, etc...)
  Finish: The pipe ends (most of the time because an error occured, wrong payload, etc...)
 *)
type 'a status =
  | Continue of 'a Context.t
  | Pass of 'a Context.t
  | Finish of Response.t
[@@bs.deriving accessors]

(**
  The kind of pipe: synchronous, or asynchronous
 *)
type 'a kind = Sync of 'a status | Async of 'a status Js.Promise.t
[@@bs.deriving accessors]

type 'a t = 'a Context.t -> 'a kind
(**
  The pipe type itself, takes a context and return a kind
 *)

let ( >=> ) : 'a. 'a t -> 'a t -> 'a t =
 fun a b ctx ->
  match a ctx with
  | Sync (Continue ctx) -> b ctx
  | (Sync (Pass _) as kind) | (Sync (Finish _) as kind) -> kind
  | Async promise ->
      async
        ( promise
        |> Js.Promise.then_ (fun status ->
               match status with
               | (Pass _ as status) | (Finish _ as status) ->
                   Js.Promise.resolve status
               | Continue ctx -> (
                   match b ctx with
                   | Sync status -> Js.Promise.resolve status
                   | Async promise -> promise )) )

let pipe = ( >=> )

let ( <|> ) : 'a. 'a t -> 'a t -> 'a t =
 fun a b ctx ->
  match a ctx with
  | (Sync (Continue _) | Sync (Finish _)) as kind -> kind
  | Sync (Pass ctx) -> b ctx
  | Async promise ->
      async
        ( promise
        |> Js.Promise.then_ (fun status ->
               match status with
               | (Continue _ | Finish _) as status -> Js.Promise.resolve status
               | Pass ctx -> (
                   match b ctx with
                   | Sync status -> Js.Promise.resolve status
                   | Async promise -> promise )) )

let alt = ( <|> )

let oneOf pipes =
  match pipes with
  (* No pipes have been provided, let's simply continue *)
  | [||] -> fun ctx -> Sync (Continue ctx)
  | [| pipe |] -> pipe
  | [| pipe1; pipe2 |] -> pipe1 <|> pipe2
  | pipes ->
      let head, tail = (pipes.(0), Js.Array2.sliceFrom pipes 1) in
      Js.Array2.reduce tail ( <|> ) head

let%private currentPathPart
    { Context.request = { pathName }; meta = { currentNamespace } } =
  let from = Js.String.length currentNamespace + 1 in
  let to_ = pathName |> Js.String.indexOfFrom "/" from in
  Js.String.substring pathName ~from
    ~to_:(if to_ < 0 then Js.String.length pathName else to_)

let setContentType : Http.ContentType.t -> 'a t =
 fun contentType ctx ->
  sync @@ continue
  @@ Context.mapResponse
       ( Response.mapHeaders @@ Headers.set "Content-Type"
       @@ Http.ContentType.show contentType )
       ctx

let setContentLength : int -> 'a t =
 fun contentLength ctx ->
  sync @@ continue
  @@
  if contentLength < 0 then ctx
  else
    Context.mapResponse
      ( Response.mapHeaders
      @@ Headers.set "Content-Length"
      @@ Belt.Int.toString contentLength )
      ctx

let setStatus : Http.Status.t -> 'a t =
 fun status ctx ->
  sync @@ continue @@ Context.mapResponse (Response.setStatus status) ctx

let setHeader : string -> string -> 'a t =
 fun key value ctx ->
  sync @@ continue
  @@
  if key = "" || value = "" then ctx
  else Context.mapResponse (Response.mapHeaders @@ Headers.set key value) ctx

let setHeaders : string Js.Dict.t -> 'a t =
 fun headers ctx ->
  sync @@ continue
  @@ Context.mapResponse
       ( Response.setHeaders @@ Js.Dict.fromArray
       @@ Js.Array.filter (fun (key, value) -> key <> "" && value <> "")
       @@ Js.Dict.entries headers )
       ctx

let setCookie ?expires ?maxAge ?domain ?path ?(secure = false)
    ?(httpOnly = false) ?sameSite name content =
  let options =
    [|
      (name, Some content);
      ("Expires", Belt.Option.map expires Js.Date.toUTCString);
      ("Max-Age", Belt.Option.map maxAge Belt.Int.toString);
      ("Domain", domain);
      ("Path", path);
      ("Secure", if secure then Some "" else None);
      ("HttpOnly", if httpOnly then Some "" else None);
      ( "SameSite",
        Belt.Option.map sameSite @@ function
        | `strict -> "Strict"
        | `lax -> "Lax"
        | `none -> "None" );
    |]
  in
  let serializeOption acc (key, value) =
    acc
    ^
    match value with
    | None -> ""
    | Some "" when acc = "" -> key
    | Some "" -> "; " ^ key
    | Some value when acc = "" -> key ^ "=" ^ value
    | Some value -> "; " ^ key ^ "=" ^ value
  in

  setHeader "Set-Cookie" (Js.Array.reduce serializeOption "" options)

let resolveUrl : 'a. ?secure:bool -> 'a t =
 fun ?(secure = true) ({ Context.request = { headers; pathName } } as ctx) ->
  let host = Headers.get "host" headers in
  let protocol = if secure then "https://" else "http://" in
  let url =
    Belt.Option.map
      (Belt.Option.flatMap host (fun host ->
           Bindings.Node.Url.make pathName (protocol ^ host)))
      Bindings.Node.Url.href
  in
  sync @@ continue @@ Context.mapRequest (Request.setUrl url) ctx

let when_ : 'a 'b. 'b option -> ('b -> 'a t) -> 'a t =
 fun option pipe ctx ->
  match option with
  | None -> sync @@ continue ctx
  | Some value -> pipe value ctx

let%private handlePayload :
              'a 'b. ('b -> 'a t) -> ('b, Serializable.t) result -> 'a t =
 fun f decodedBody ctx ->
  match decodedBody with
  | Ok payload -> f payload ctx
  | Error errors ->
      sync @@ finish @@ Response.make ~status:`badRequest ~body:errors ()

let payload :
      'a 'b. (string -> ('b, Serializable.t) result) -> ('b -> 'a t) -> 'a t =
 fun decode f ({ Context.request = { body } } as ctx) ->
  match body with
  | None -> sync @@ finish @@ Response.make ~status:`badRequest ()
  | Some (String body) -> (handlePayload f @@ decode body) ctx
  | Some (Buffer body) ->
      ( handlePayload f @@ decode
      @@ Bindings.Node.Buffer.toStringWithEncoding body `utf8 )
        ctx
  | Some (Stream body) ->
      async
        ( Bindings.Node.Stream.Readable.consume body
        |> Js.Promise.then_ (fun body ->
               match decode body with
               | Error errors ->
                   Js.Promise.resolve @@ finish
                   @@ Response.make ~status:`badRequest ~body:errors ()
               | Ok payload -> (
                   match f payload ctx with
                   | Sync kind -> Js.Promise.resolve kind
                   | Async promise -> promise )) )

(** Capture a path part and since it's a matching pipe it will set the status to ok *)
let capture : 'a. (string -> 'a t) -> 'a t =
 fun f ctx ->
  match currentPathPart ctx with
  | "" -> sync @@ pass ctx
  | pathPart -> f pathPart ctx

let verb : 'a. Http.Verb.t -> 'a t =
 fun verb ctx ->
  if ctx.request.verb = verb then sync @@ continue ctx else sync @@ pass ctx

let route : 'a. string -> 'a t =
 fun pathPart ctx ->
  if currentPathPart ctx <> pathPart then sync @@ pass ctx
  else
    sync @@ continue
    @@ ( Context.mapMeta @@ Meta.mapCurrentNamespace
       @@ fun currentNamespace -> currentNamespace ^ "/" ^ pathPart )
         ctx

let%private respondWith : 'a. Serializable.t -> 'a t =
 fun body ->
  (fun ctx ->
    sync @@ continue @@ Context.mapResponse (Response.setBody body) ctx)
  >=> fun ctx ->
  setStatus (Belt.Option.getWithDefault ctx.Context.response.status `ok) ctx

let text text =
  setContentType `text
  >=> setContentLength (Js.String.length text)
  >=> respondWith @@ Serializable.fromString text

let status status =
  setStatus status
  >=> when_
        (Serializable.length @@ Serializable.fromStatus status)
        setContentLength
  >=> respondWith @@ Serializable.fromStatus status

let json json =
  setContentType `json
  >=> when_ (Serializable.length @@ Serializable.fromJson json) setContentLength
  >=> respondWith @@ Serializable.fromJson json

let redirect url = setHeader "Location" url >=> setStatus `found
