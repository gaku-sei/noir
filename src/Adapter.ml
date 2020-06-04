module type Type = sig
  val listen : (Request.t -> Response.t Js.Promise.t) -> unit
end
