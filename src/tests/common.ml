(** This module provide all the common testing infrastructure. It is intended to be opened in
    all testing modules *)
module Q = QCheck

module Gen = struct
  include Q.Gen

  let ( let+ ) o f = map f o

  let ( and+ ) = pair

  let ( let* ) o f = o >>= f

  let ( and* ) = pair
end

module QCT = Q.Test
module Print = Q.Print

let ( ==> ) = QCheck.( ==> )
