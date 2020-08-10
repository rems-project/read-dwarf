(** This module contain all the definition to handle local and global variables
    as defined in the DWARF information of the target file *)

(** Type of a DWARF variable *)
type t = { name : string; param : bool; ctype : Ctype.t; locs : ((int * int) * Loc.t) list }

(** Type of a DWARF variable in linksem *)
type linksem_t = Dwarf.sdt_variable_or_formal_parameter

(** Merge contiguous location lists *)
let rec loc_merge = function
  | ((a1, b1), d1) :: ((a2, b2), d2) :: l when b1 = a2 && Loc.compare d1 d2 = 0 ->
      loc_merge (((a1, b2), d1) :: l)
  | a :: l -> a :: loc_merge l
  | [] -> []

(** Convert from Z.t to int, if there is an overflow, returns Int.max_int instead of throwing *)
let clamp_z z = try Z.to_int z with Z.Overflow when Z.compare z Z.zero > 0 -> Int.max_int

(** Create a DWARF variable from its linksem counterpart *)
let of_linksem (elf : Elf.File.t) (env : Ctype.env) (lvar : linksem_t) : t =
  let name = lvar.svfp_name in
  let param = match lvar.svfp_kind with SVPK_var -> false | SVPK_param -> true in
  let ctype = match lvar.svfp_type with Some t -> Ctype.of_linksem ~env t | None -> Ctype.of_linksem_none () in
  let locs =
    lvar.svfp_locations |> Option.value ~default:[]
    |> List.map (fun (a, b, l) -> ((Z.to_int a, clamp_z b), Loc.of_linksem elf l))
    |> loc_merge
  in
  { name; param; ctype; locs }

(** Pretty print a variable *)
let pp_raw v =
  let kind = if v.param then "param" else "var" in
  PP.(
    record kind
      [
        ("name", string v.name);
        ("ctype", Ctype.pp v.ctype);
        ("locs", list (pair (pair ptr ptr) Loc.pp) v.locs);
      ])
