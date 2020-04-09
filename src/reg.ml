(* The documentation is in the mli file *)

type t = int

type typ = Plain of Isla.ty | Struct of reg_struct

and reg_struct = (string, typ) IdMap.t

let make_struct () = IdMap.make ()

let assert_plain : typ -> Isla.ty = function
  | Plain t -> t
  | Struct _ -> failwith "assert_plain failed"

let index = make_struct ()

(*****************************************************************************)
(*        Accessors                                                          *)
(*****************************************************************************)

let field_to_string = IdMap.of_ident

let to_string = IdMap.of_ident index

let field_of_string = IdMap.to_ident

let of_string = IdMap.to_ident index

let mem = IdMap.mem_id index

let mem_string = IdMap.mem index

let num_field = IdMap.length

let num_reg () = num_field index

let field_type = IdMap.geti

let reg_type = field_type index

(*****************************************************************************)
(*        Equality                                                           *)
(*****************************************************************************)

(** Just an internal exception for control-flow purposes *)
exception Nope

let rec struct_weak_inc_Nope s s' =
  let field name _ typ =
    let typ' = Opt.value_fun ~default:(fun _ -> raise Nope) @@ IdMap.getk_opt s' name in
    type_weak_inc_Nope typ typ'
  in
  IdMap.iter field s

and type_weak_inc_Nope t t' =
  match (t, t') with
  | (Plain i, Plain j) when i = j -> ()
  | (Struct rs, Struct rs') -> struct_weak_inc_Nope rs rs'
  | _ -> raise Nope

let struct_weak_inc s s' =
  try
    struct_weak_inc_Nope s s';
    true
  with Nope -> false

let type_weak_inc t t' =
  try
    type_weak_inc_Nope t t';
    true
  with Nope -> false

let struct_weak_eq s s' = struct_weak_inc s s' && struct_weak_inc s' s

let type_weak_eq t t' = type_weak_inc t t' && type_weak_inc t' t

(*****************************************************************************)
(*        Adding                                                             *)
(*****************************************************************************)

let add_field = IdMap.add

let adds_field = IdMap.adds

let add name typ = add_field index name typ

let adds name typ = adds_field index name typ

let rec struct_merge_add rs rs' =
  assert (struct_weak_inc rs rs');
  IdMap.iter
    (fun name _ typ ->
      match IdMap.getk_opt rs name with
      | Some typ0 -> type_merge_add typ0 typ
      | None -> IdMap.adds rs name typ)
    rs'

and type_merge_add t t' =
  assert (type_weak_inc t t');
  if type_weak_eq t t' then ()
  else
    match (t, t') with
    | (Struct rs, Struct rs') -> struct_merge_add rs rs'
    | _ -> failwith "broken invariant"

(*****************************************************************************)
(*        Path manipulation                                                  *)
(*****************************************************************************)

(* TODO Put path in Reg.Path *)

type path = t list

let empty_path = []

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

let pp_path path = path |> path_to_string |> PP.string

(*****************************************************************************)
(*        Register indexed mapping                                           *)
(*****************************************************************************)

(* Doc for Reg.Map in the mli file *)
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
      Array.init (num_field rs) (fun i -> initc (root @ [i]) f (field_type rs i))
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

  (* TODO try not to use exceptions in normal control flow paths *)
  let get_opt map path = try Some (get map path) with Failure _ -> None

  let get_or ~value map path = match get_opt map path with Some a -> a | None -> value

  let rec map f m =
    let mapcell = function MPlain a -> MPlain (f a) | MStruct m -> MStruct (map f m) in
    Array.map mapcell m

  (* TODO move that into a proper array extension *)
  let array_map_mut f a =
    let len = Array.length a in
    for i = 0 to len - 1 do
      Array.unsafe_get a i |> f |> Array.unsafe_set a i
    done

  let rec map_mut f m =
    let map_mut_cell = function
      | MPlain a -> MPlain (f a)
      | MStruct m as ms ->
          map_mut f m;
          ms
    in
    array_map_mut map_mut_cell m

  let copy m = map Fun.id m

  let rec iter f m =
    let mapcell = function MPlain a -> f a | MStruct m -> iter f m in
    Array.iter mapcell m

  let iteri f m =
    let rec iteri_aux f path m =
      let mapcell i = function
        | MPlain a -> f (path @ [i]) a
        | MStruct m -> iteri_aux f (path @ [i]) m
      in
      Array.iteri mapcell m
    in
    iteri_aux f [] m

  let copy_extend ~init m =
    let rec cecell (ty : typ) ?prev (root : path) =
      match (ty, prev) with
      | (Plain _, Some (MPlain v)) -> MPlain v
      | (Plain _, None) -> MPlain (init root)
      | (Struct rs, Some (MStruct prev)) -> MStruct (cestruct rs ~prev root)
      | (Struct rs, None) -> MStruct (cestruct rs root)
      | _ -> failwith "copy_extend: corrupted Reg.Map"
    and cestruct rs ?prev (root : path) =
      let new_init prev index = cecell (field_type rs index) ?prev (root @ [index]) in
      let init : int -> 'a =
        match prev with
        | None -> new_init None
        | Some arr ->
            let old_len = Array.length arr in
            fun i -> if i < old_len then new_init (Some arr.(i)) i else new_init None i
      in
      Array.init (num_field rs) init
    in
    cestruct index ~prev:m []

  let rec pp_struct rs conv rm : PP.document =
    rm
    |> Array.mapi (fun i c -> (field_to_string rs i, pp_plain (field_type rs i) conv c))
    |> Array.to_list |> PP.OCaml.record ""

  and pp_plain rt conv cell =
    match (rt, cell) with
    | (Plain _, MPlain a) -> conv a
    | (Struct rs, MStruct s) -> pp_struct rs conv s
    | _ -> failwith "Reg.Map corruption"

  let pp conv cell = pp_struct index conv cell
end

(*****************************************************************************)
(*        Pretty Printing                                                    *)
(*****************************************************************************)

let pp reg = reg |> to_string |> PP.string

let pp_field rs reg = reg |> field_to_string rs |> PP.string

let rec pp_rstruct rs = IdMap.pp ~name:"struct" ~keys:PP.string ~vals:pp_rtype rs

and pp_rtype = PP.(function Plain i -> pp_ty i | Struct s -> pp_rstruct s)
