module Status = struct
  type t =
    (* 2xx *)
    [ `ok
    | `created
    | `accepted
    | `nonAuthoritativeInformation
    | `noContent
    | `resetContent
    | `partialContent
    | `multiStatus
    | `alreadyReported
    | `imUsed
    | (* 3xx *)
      `mulitpleChoices
    | `movedPermanently
    | `found
    | `seeOther
    | `notModified
    | `useProxy
    | `switchProxy
    | `temporaryRedirect
    | `permanentRedirect
    | (* 4xx *)
      `badRequest
    | `unauthorized
    | `paymentRequired
    | `forbidden
    | `notFound
    | `methodNotAllowed
    | `notAcceptable
    | `proxyAuthentificationRequired
    | `requestTimeout
    | `conflict
    | `gone
    | `lengthRequired
    | `preconditionFailed
    | `payloadTooLarge
    | `uriTooLong
    | `unsupportedMediaType
    | `rangeNotSatisfiable
    | `exceptionFailed
    | `imATeapot
    | `misdirectedRequest
    | `unprocessableEntity
    | `locked
    | `failedDependency
    | `tooEarly
    | `upgradeRequired
    | `preconditionRequired
    | `tooManyRequests
    | `requestHeaderFieldsTooLarge
    | `unavailableForLegalReasons
    | (* 5xx *)
      `internalServerError
    | `notImplemented
    | `badGateway
    | `serviceUnavailable
    | `gatewayTimeout
    | `httpVersionNotSupported
    | `variantAlsoNegotiates
    | `insufficientStorage
    | `loopDetected
    | `notExtended
    | `networkAuthentificationRequired ]

  let toCode : t -> int = function
    | `ok -> 200
    | `created -> 201
    | `accepted -> 202
    | `nonAuthoritativeInformation -> 203
    | `noContent -> 204
    | `resetContent -> 205
    | `partialContent -> 206
    | `multiStatus -> 207
    | `alreadyReported -> 208
    | `imUsed -> 226
    | `mulitpleChoices -> 300
    | `movedPermanently -> 301
    | `found -> 302
    | `seeOther -> 303
    | `notModified -> 304
    | `useProxy -> 305
    | `switchProxy -> 306
    | `temporaryRedirect -> 307
    | `permanentRedirect -> 308
    | `badRequest -> 400
    | `unauthorized -> 401
    | `paymentRequired -> 402
    | `forbidden -> 403
    | `notFound -> 404
    | `methodNotAllowed -> 405
    | `notAcceptable -> 406
    | `proxyAuthentificationRequired -> 407
    | `requestTimeout -> 408
    | `conflict -> 409
    | `gone -> 410
    | `lengthRequired -> 411
    | `preconditionFailed -> 412
    | `payloadTooLarge -> 413
    | `uriTooLong -> 414
    | `unsupportedMediaType -> 415
    | `rangeNotSatisfiable -> 416
    | `exceptionFailed -> 417
    | `imATeapot -> 418
    | `misdirectedRequest -> 421
    | `unprocessableEntity -> 422
    | `locked -> 423
    | `failedDependency -> 424
    | `tooEarly -> 425
    | `upgradeRequired -> 426
    | `preconditionRequired -> 428
    | `tooManyRequests -> 429
    | `requestHeaderFieldsTooLarge -> 431
    | `unavailableForLegalReasons -> 451
    | `internalServerError -> 500
    | `notImplemented -> 501
    | `badGateway -> 502
    | `serviceUnavailable -> 503
    | `gatewayTimeout -> 504
    | `httpVersionNotSupported -> 505
    | `variantAlsoNegotiates -> 506
    | `insufficientStorage -> 507
    | `loopDetected -> 508
    | `notExtended -> 510
    | `networkAuthentificationRequired -> 511

  let toMessage : t -> string = function
    | `ok -> "ok"
    | `created -> "created"
    | `accepted -> "accepted"
    | `nonAuthoritativeInformation -> "non authoritative information"
    | `noContent -> "no content"
    | `resetContent -> "reset content"
    | `partialContent -> "partial content"
    | `multiStatus -> "multi status"
    | `alreadyReported -> "already reported"
    | `imUsed -> "im used"
    | `mulitpleChoices -> "multiple choices"
    | `movedPermanently -> "moved permanently"
    | `found -> "found"
    | `seeOther -> "see other"
    | `notModified -> "not modified"
    | `useProxy -> "use proxy"
    | `switchProxy -> "switch proxy"
    | `temporaryRedirect -> "temporary redirect"
    | `permanentRedirect -> "permanent redirect"
    | `badRequest -> "bad request"
    | `unauthorized -> "unauthorized"
    | `paymentRequired -> "payment required"
    | `forbidden -> "forbidden"
    | `notFound -> "not found"
    | `methodNotAllowed -> "method not allowed"
    | `notAcceptable -> "not acceptable"
    | `proxyAuthentificationRequired -> "proxy authentification required"
    | `requestTimeout -> "request timeout"
    | `conflict -> "conflict"
    | `gone -> "gone"
    | `lengthRequired -> "length required"
    | `preconditionFailed -> "precondition failed"
    | `payloadTooLarge -> "payload too large"
    | `uriTooLong -> "uri too long"
    | `unsupportedMediaType -> "unsupported media type"
    | `rangeNotSatisfiable -> "range not safisfiable"
    | `exceptionFailed -> "exception failed"
    | `imATeapot -> "i'm a teapot"
    | `misdirectedRequest -> "misdirected request"
    | `unprocessableEntity -> "unprocessable entity"
    | `locked -> "locked"
    | `failedDependency -> "failed dependency"
    | `tooEarly -> "too ealry"
    | `upgradeRequired -> "upgrade required"
    | `preconditionRequired -> "precondition required"
    | `tooManyRequests -> "too many requests"
    | `requestHeaderFieldsTooLarge -> "request header fields too large"
    | `unavailableForLegalReasons -> "unavaible for legal reasons"
    | `internalServerError -> "internal serer error"
    | `notImplemented -> "not implemented"
    | `badGateway -> "bad gateway"
    | `serviceUnavailable -> "service unavailable"
    | `gatewayTimeout -> "gateway timeout"
    | `httpVersionNotSupported -> "http version not supported"
    | `variantAlsoNegotiates -> "variant also negotiates"
    | `insufficientStorage -> "insufficient storage"
    | `loopDetected -> "loop detected"
    | `notExtended -> "not exgtended"
    | `networkAuthentificationRequired -> "network authentification required"
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
