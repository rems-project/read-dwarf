(** Extension of the string module of the standard library *)

include Stdlib.String

(** Check if a predicate hold for all [char] in a string *)
let for_all f s =
  let len = length s in
  let rec for_all_from i =
    if i >= len then true else if f s.[i] then for_all_from (i + 1) else false
  in
  for_all_from 0

(** Check if a predicate hold for at least one [char] in a string *)
let exists f s =
  let len = length s in
  let rec exists_from i =
    if i >= len then false else if f s.[i] then true else exists_from (i + 1)
  in
  exists_from 0
