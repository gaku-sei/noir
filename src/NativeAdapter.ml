type request = { headers : string Js.Dict.t; url : string }

type response = { mutable statusCode : int; mutable statusMessage : string }

external write : response -> Serializable.t -> unit = "write" [@@bs.send]

external _end : response -> unit = "end" [@@bs.send]

external setHeader : response -> string -> string -> unit = "setHeader"
  [@@bs.send]

type t

external createServer' : (request -> response -> unit) -> t = "createServer"
  [@@bs.module "http"]

external listen' : t -> int -> unit = "listen" [@@bs.send]

let listen f =
  let server =
    createServer' @@ fun { headers; url } response ->
    f (Request.make ~headers ~pathName:url ~url ())
    |> Js.Promise.then_ (fun ({ body; headers; status } : Response.t) ->
           (* Set status *)
           response.statusCode <- Http.Status.toCode status;
           response.statusMessage <- Http.Status.toMessage status;
           (* Set headers *)
           headers |> Js.Dict.entries
           |> Js.Array.forEach (fun (key, value) ->
                  setHeader response key value);
           (* Write response content *)
           write response
           @@ Belt.Option.getWithDefault body
           @@ Serializable.fromStatus status;
           (* End request *)
           _end response;
           Js.Promise.resolve ())
    |> ignore
  in

  listen' server 3000
