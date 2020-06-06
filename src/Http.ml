module Status = struct
  type t = [ `ok | `noContent | `notFound ]

  let toCode : t -> int = function
    | `ok -> 200
    | `noContent -> 204
    | `notFound -> 404

  let toMessage : t -> string = function
    | `ok -> "ok"
    | `noContent -> "no content"
    | `notFound -> "not found"
end

module ContentType = struct
  type t = [ `json | `text ]

  let show : t -> string = function
    | `json -> "application/json"
    | `text -> "text/html"
end

module Verb = struct
  type t =
    [ `head
    | `options
    | `trace
    | `connect
    | `get
    | `post
    | `put
    | `delete
    | `patch ]
end
