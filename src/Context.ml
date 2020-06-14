type 'a t = {
  config : 'a;
  meta : Meta.t;
  request : Request.t;
  response : Response.t;
}
[@@bs.deriving accessors]

let make ~config ~request ~response =
  { config; meta = Meta.make (); request; response }

let mapMeta f ({ meta } as ctx) = { ctx with meta = f meta }

let mapResponse f ({ response } as ctx) = { ctx with response = f response }

let mapRequest f ({ request } as ctx) = { ctx with request = f request }
