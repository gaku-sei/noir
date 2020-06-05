type t = { meta : Meta.t; request : Request.t; response : Response.t }

let make ~request ~response = { meta = Meta.make (); request; response }

let mapMeta f ({ meta } as ctx) = { ctx with meta = f meta }

let mapResponse f ({ response } as ctx) = { ctx with response = f response }

let mapRequest f ({ request } as ctx) = { ctx with request = f request }
