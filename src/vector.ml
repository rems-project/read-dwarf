(** This module is a rename of Res.array for the day we want to remove the Res dependency *)
include Res.Array

(** ensure vec size v make the vector at least size size. Add value v at the end *)
let ensure vec size v =
  let len = length vec in
  if size > len then
    for _ = len to size - 1 do
      add_one vec v
    done
  else ()

(** resize vec size v make the vector of size size.
    If this is an expansion, add value v to the end *)
let resize vec size v =
  let len = length vec in
  if size < len then remove_n vec (len - size) else ensure vec size v

let map2 f veca vecb =
  let arra = to_array veca and arrb = to_array vecb in
  Array.map2 f arra arrb |> of_array

(** Vector pretty printer *)
let pp conv vec = PP.(!^"vec" ^^ (vec |> to_array |> array conv))

let vec_length_test () =
  let v = empty () in
  add_one v 1;
  add_one v 1;
  length v = 2

let _ = Tests.add_test "Vector.length" vec_length_test
