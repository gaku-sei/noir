type t = {
  body : Serializable.t option;
  headers : Headers.t;
  status : Http.Status.t option;
}
[@@bs.deriving accessors]

let make ?(headers = Js.Dict.empty ()) ?status ?body () =
  { body; headers; status }

let ok = make ~status:`ok

let notFound = make ~status:`notFound

let mapHeaders f ({ headers } as response) =
  { response with headers = f headers }

let setHeaders headers response = { response with headers }

let setStatus status response = { response with status = Some status }

let setBody body response = { response with body = Some body }
