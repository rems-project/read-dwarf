(** This module handle the register abstraction:
    - map string to identifiers
    - remember register characteristics (for now only bit size)
    - remember structure of register (like PSTATE)
*)

(** The type of a register. users can do type reg = Reg.t*)
type t = int

type typ = Plain of Isla.ty | Struct of reg_struct

and reg_struct = { fields : string Bimap.t; types : typ Vector.t }

let make_struct () = { fields = Bimap.make (); types = Vector.empty () }

let assert_plain : typ -> Isla.ty = function
  | Plain t -> t
  | Struct _ -> failwith "assert_plain failed"

(** The set of register is represented as a massive structure*)
let index = make_struct ()

(** register map *)
let rmap = index.fields

let field_to_string rs reg = Bimap.of_ident rs.fields reg

let to_string reg = Bimap.of_ident rmap reg

let field_of_string rs s = Bimap.to_ident rs.fields s

let of_string s = Bimap.to_ident rmap s

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
    match (field_type s i, field_type s' (field_of_string s' n)) with
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

(** Add a new register by name and throws Bimap.Exists if it already exists. return Reg.t*)
let add_reg name typ = add_field index name typ

(*****************************************************************************)
(*        Path manipulation                                                  *)
(*****************************************************************************)

(* TODO Put path in Reg.Path *)

type path = t list

let rec partial_path_to_string_list rs = function
  | [] -> []
  | f :: l -> (
      let fname = field_to_string rs f in
      match field_type rs f with
      | Plain _ -> [fname]
      | Struct rs' -> fname :: partial_path_to_string_list rs' l
    )

let partial_path_to_string rs path = String.concat "." @@ partial_path_to_string_list rs path

let path_to_string_list = partial_path_to_string_list index

let path_to_string = partial_path_to_string index

let rec partial_path_of_string_list rs = function
  | [] -> []
  | f :: l -> (
      let fid = field_of_string rs f in
      match field_type rs fid with
      | Plain _ -> [fid]
      | Struct rs' -> fid :: partial_path_of_string_list rs' l
    )

let partial_path_of_string rs s = partial_path_of_string_list rs @@ String.split_on_char '.' s

let path_of_string = partial_path_of_string index

let path_of_string_list = partial_path_of_string_list index

let rec partial_path_type rs = function
  | [] -> failwith "partial_path_type: empty list"
  | [f] -> field_type rs f
  | f :: l -> (
      match field_type rs f with
      | Plain _ -> failwith "partial_path_type: invalid path"
      | Struct rs' -> partial_path_type rs' l
    )

let path_type = partial_path_type index

(*****************************************************************************)
(*        Register indexed mapping                                           *)
(*****************************************************************************)

module Map = struct
  type 'a cell = MPlain of 'a | MStruct of 'a t

  and 'a t = 'a cell array

  let is_plain = function MPlain _ -> true | _ -> false

  let dummy () = Array.make 0 (Obj.magic ())

  let init f =
    let rec initc root f = function
      | Plain _ -> MPlain (f root)
      | Struct rs -> MStruct (initm root f rs)
    and initm root f rs =
      rs.types |> Vector.mapi (fun i t -> initc (root @ [i]) f t) |> Vector.to_array
    in
    initm [] f index

  let rec get_mut_cell map (path : path) =
    let gcellc cell path =
      match cell with
      | MPlain _ -> failwith "get_cell error, path too long"
      | MStruct map -> get_mut_cell map path
    in
    match path with
    | [a] -> ArrayCell.make map a
    | a :: l -> gcellc map.(a) l
    | [] -> failwith "get_cell error, path too short"

  let get_cell map path = get_mut_cell map path |> ArrayCell.get

  let get map path =
    match get_cell map path with
    | MPlain a -> a
    | MStruct _ -> failwith "Reg.Map.get : path too short"

  let set map path a =
    let cell = get_mut_cell map path in
    assert (is_plain @@ ArrayCell.get cell);
    ArrayCell.set cell (MPlain a)

  let rec map f m =
    let mapcell = function MPlain a -> MPlain (f a) | MStruct m -> MStruct (map f m) in
    Array.map mapcell m

  let copy m = map Fun.id m

  let rec iter f m =
    let mapcell = function MPlain a -> f a | MStruct m -> iter f m in
    Array.iter mapcell m

  let rec pp_struct rs conv rm : PP.document =
    rm
    |> Array.mapi (fun i c -> (field_to_string rs i, pp_plain (field_type rs i) conv c))
    |> Array.to_list |> PP.OCaml.record ""

  and pp_plain rt conv cell =
    match (rt, cell) with
    | Plain _, MPlain a -> conv a
    | Struct rs, MStruct s -> pp_struct rs conv s
    | _ -> failwith "Reg.Map corruption"

  let pp conv cell = pp_struct index conv cell
end

(*****************************************************************************)
(*        Pretty Printing                                                    *)
(*****************************************************************************)

let pp reg = reg |> to_string |> PP.string

let pp_field rs reg = reg |> field_to_string rs |> PP.string

let rec pp_rstruct rs =
  PP.(
    surround 2 0 !^"struct{"
      (Vector.map2
         (fun name typ -> infix 2 1 !^":" (string name) (pp_rtype typ))
         (Bimap.vec rs.fields) rs.types
      |> Vector.to_list
      |> separate (semi ^^ space)
      )
      !^"}")

and pp_rtype = PP.(function Plain i -> !^"Plain " ^^ pp_ty i | Struct s -> pp_rstruct s)
