(** This module is for extending the [Array] module of the standard library *)

include Stdlib.Array

let map_mut f arr =
  let len = length arr in
  for i = 0 to len - 1 do
    unsafe_get arr i |> f |> unsafe_set arr i
  done
