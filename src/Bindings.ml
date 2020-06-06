module Node = struct
  include Node

  module Stream = struct
    module Readable = struct
      type t

      let consume : t -> string Js.Promise.t =
        [%raw
          {|
function toString(stream) {
  var content = '';

  return new Promise(function promise(resolve, reject) {
    stream.on('data', function onData(chunk) {
      content += chunk.toString('utf-8');
    });

    stream.on('error', function onError(error) {
      reject(error);
    });

    stream.on('end', function onEnd() {
      resolve(content);
    });
  });
}
        |}]
    end
  end
end
