(* The documentation is in the mli file *)

open ElfSymbol

type sym = ElfSymbol.t

type linksem_sym = ElfSymbol.linksem_t

type sym_offset = sym * int

module IMap = Map.Make (Int)
module SMap = Map.Make (String)

type linksem_t = Elf_file.global_symbol_init_info

type t = { by_name : sym SMap.t; by_addr : sym IMap.t }

let empty = { by_name = SMap.empty; by_addr = IMap.empty }

let add t sym =
  { by_name = SMap.add sym.name sym t.by_name; by_addr = IMap.add sym.addr sym t.by_addr }

let of_name t name = SMap.find name t.by_name

let of_addr t addr =
  let candidate = IMap.find_last (fun a -> a <= addr) t.by_addr |> snd in
  if is_in candidate addr then candidate else raise Not_found

let of_addr_opt t addr =
  let candidate = IMap.find_last (fun a -> a <= addr) t.by_addr |> snd in
  if is_in candidate addr then Some candidate else None

let of_addr_with_offset t addr =
  let sym = of_addr t addr in
  (sym, addr - sym.addr)

let string_of_sym_offset ((sym, off) : sym_offset) = sym.name ^ "+" ^ string_of_int off

let sym_offset_of_string t s : sym_offset =
  match String.split_on_char '+' s with
  | [ssym] -> (of_name t (String.trim ssym), 0)
  | [ssym; soff] -> (of_name t (String.trim ssym), soff |> String.trim |> int_of_string)
  | _ -> failwith "sym_offset_to_string: wrong format"

let of_linksem segments linksem_map =
  let add_linksem_sym_to_map (map : t) (lsym : linksem_sym) =
    if is_interesting_linksem lsym then add map (ElfSymbol.of_linksem segments lsym) else map
  in
  List.fold_left add_linksem_sym_to_map empty linksem_map

let pp_raw st = IMap.bindings st.by_addr |> List.map (Pair.map PP.ptr pp_raw) |> PP.mapping
