type t = {
  body : Serializable.t option;
  headers : Headers.t;
  status : Http.Status.t;
}

let make ?(headers = Js.Dict.empty ()) ?(status = `ok) ?body () =
  { body; headers; status }

let empty = make ()

let mapHeaders f ({ headers } as response) =
  { response with headers = f headers }

let setHeaders headers response = { response with headers }

let setStatus status response = { response with status }

let setBody body response = { response with body }
