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
  type t = [ `json ]

  let show : t -> string = function `json -> "application/json"
end
