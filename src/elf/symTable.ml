(* The documentation is in the mli file *)

open Symbol

type sym = Symbol.t

type linksem_sym = Symbol.linksem_t

type sym_offset = sym * int

module RMap = RngMap.Make (Symbol)
module SMap = Map.Make (String)

type linksem_t = Elf_file.global_symbol_init_info

type t = { by_name : sym SMap.t; by_addr : RMap.t }

let empty = { by_name = SMap.empty; by_addr = RMap.empty }

let add t sym =
  let by_name = SMap.add sym.name sym t.by_name in
  try { by_name; by_addr = RMap.add t.by_addr sym.addr sym }
  with Invalid_argument _ ->
    let updated = ref false in
    let by_addr =
      RMap.update
        (fun usym ->
          if usym.addr = sym.addr && usym.size = sym.size then begin
            updated := true;
            push_name sym.name usym
          end
          else Raise.fail "Failed to merge %s in %s" usym.name sym.name)
        t.by_addr sym.addr
    in
    if !updated then { by_name; by_addr }
    else Raise.fail "Coudln't insert symbol %s, it didn't fit in" sym.name

let of_name t name = SMap.find name t.by_name

let of_name_opt t name = SMap.find_opt name t.by_name

let of_addr t addr = RMap.at t.by_addr addr

let of_addr_opt t addr = RMap.at_opt t.by_addr addr

let of_addr_with_offset t addr = RMap.at_off t.by_addr addr

let of_addr_with_offset_opt t addr = RMap.at_off_opt t.by_addr addr

let to_addr_offset (sym, offset) = sym.addr + offset

let string_of_sym_offset ((sym, off) : sym_offset) = sym.name ^ "+" ^ string_of_int off

let sym_offset_of_string t s : sym_offset =
  match String.split_on_char '+' s with
  | [ssym] -> (of_name t (String.trim ssym), 0)
  | [ssym; soff] -> (of_name t (String.trim ssym), soff |> String.trim |> int_of_string)
  | _ -> Raise.fail "sym_offset_to_string: wrong format: %s" s

let of_position_string t s : sym_offset =
  let s = String.trim s in
  if s = "" then raise Not_found;
  if s.[0] = '0' then of_addr_with_offset t (int_of_string s) else sym_offset_of_string t s

let of_linksem segments linksem_map =
  let add_linksem_sym_to_map (map : t) (lsym : linksem_sym) =
    if is_interesting_linksem lsym then add map (Symbol.of_linksem segments lsym) else map
  in
  List.fold_left add_linksem_sym_to_map empty linksem_map

let pp_raw st = RMap.bindings st.by_addr |> List.map (Pair.map Pp.ptr pp_raw) |> Pp.mapping "syms"