(** Extension of the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html}[String]}
    module of the standard library. *)

include Stdlib.String

(** Check if a predicate hold for all [char] in a string *)
let for_all f s =
  let len = length s in
  let rec for_all_from i =
    if i >= len then true else if f (unsafe_get s i) then for_all_from (i + 1) else false
  in
  for_all_from 0

(** Check if a predicate hold for at least one [char] in a string *)
let exists f s =
  let len = length s in
  let rec exists_from i =
    if i >= len then false else if f (unsafe_get s i) then true else exists_from (i + 1)
  in
  exists_from 0

(** Convert the list into a list of char. Probably a bad idea on large strings. *)
let to_list s =
  let len = length s in
  let res = ref [] in
  for i = len - 1 downto 0 do
    res := unsafe_get s i :: !res
  done;
  !res