(** This module handle the register abstraction:
    - map string to identifiers
    - remember register characteristics (for now only bit size)
    - remember structure of register (like PSTATE)
*)

(** The type of a register. users can do type reg = Reg.t*)
type t = int

type typ = Plain of int | Struct of reg_struct

and reg_struct = { fields : string Bimap.t; types : typ Vector.t }

let make_struct () = { fields = Bimap.make (); types = Vector.empty () }

(** The set of register is represented as a massive structure*)
let index = make_struct ()

(** register map *)
let rmap = index.fields

let field_to_string rs reg = Bimap.from_ident rs.fields reg

let to_string reg = Bimap.from_ident rmap reg

let field_from_string rs s = Bimap.to_ident rs.fields s

let from_string s = Bimap.to_ident rmap s

let mem reg = Bimap.mem_id rmap reg

let mem_string s = Bimap.mem rmap s

let num_field rs = Vector.length rs.types

let num_reg () = num_field index

let field_type rs field = Vector.get rs.types field

let reg_type = field_type index

(*****************************************************************************)
(*        Equality                                                           *)
(*****************************************************************************)

(** Check that s is included in s' *)
let rec struct_weak_inc s s' =
  let exception Nope in
  let field n i =
    match (field_type s i, field_type s' (field_from_string s' n)) with
    | Plain i, Plain j when i = j -> ()
    | Struct rs, Struct rs' -> if not @@ struct_weak_inc rs rs' then raise Nope
    | _ -> raise Nope
  in
  try
    Bimap.iter field s.fields;
    true
  with Nope -> false

(** Check that all field in s and s' match and have same types (Reg.t value may differ *)
let struct_weak_eq s s' = struct_weak_inc s s' && struct_weak_inc s' s

let type_weak_eq t t' =
  match (t, t') with
  | Plain n, Plain m when n = m -> true
  | Struct rs, Struct rs' when struct_weak_eq rs rs' -> true
  | _ -> false

(*****************************************************************************)
(*        Adding                                                             *)
(*****************************************************************************)

let add_field rs name typ =
  let id = Bimap.add_ident rs.fields name in
  assert (id = Vector.length rs.types);
  Vector.add_one rs.types typ;
  id

let add_plain_field rs name size = add_field rs name (Plain size)

let add_reg name typ = add_field index name typ

(** Add a new register by name and throws Bimap.Exists if it already exists. return Reg.t*)
let add_plain_reg name size = add_reg name (Plain size)

(*****************************************************************************)
(*        Pretty Printing                                                    *)
(*****************************************************************************)

module PP = struct
  open PP

  let reg reg = reg |> to_string |> string

  let field rs reg = reg |> field_to_string rs |> string

  let rec rstruct rs =
    surround 2 0 !^"struct{"
      (Vector.map2
         (fun name typ -> infix 2 1 !^":" (string name) (rtype typ))
         rs.fields.from_ident rs.types
      |> Vector.to_list
      |> separate (semi ^^ space)
      )
      !^"}"

  and rtype = function Plain i -> !^"Plain " ^^ int i | Struct s -> rstruct s
end
