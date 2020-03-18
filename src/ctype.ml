(** This module represent ctypes as they are represented internally

    WIP, not documented see notes011-2020-03-03-types.
*)

module SMap = Map.Make (String)

let vDW_ATE_address = "DW_ATE_address" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_boolean = "DW_ATE_boolean" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_signed = "DW_ATE_signed" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_signed_char = "DW_ATE_signed_char" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_unsigned = "DW_ATE_unsigned" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_unsigned_char = "DW_ATE_unsigned_char" |> Dwarf.base_type_attribute_encode |> Z.to_int

(* type enum_id = int
 *
 * type structure_id = int
 *
 * type fragment_id = int
 *
 * (\** TODO This need to be specified by the architecture *\)
 * let ptrsize = 8
 *
 *
 * type unqualified =
 *   | Machine of int  (\** Size in bytes for now*\)
 *   | Cint of { name: string; signed: bool; size: int; ischar: bool }
 *   | Cbool
 *   | Ptr of { fragment: fragment; offset : offset}
 *   | Struct of structure_id
 *   | Array of { elem: t; dims: int option list }
 *   | Enum of enum_id (\* Keep them separate, but coercible *\)
 *
 * and t = { unqualified: unqualified; const: bool; volatile: bool; restrict: bool; constexpr: bool }
 *
 * and fragment =
 *   | Unknown (\** Unknown type, without fragment. *\)
 *   | Single of t (\* Single type, for accessing of a global (on constexpr uint to pointer cast) *\)
 *   | DynArray of t
 *   | FreeLayout of fragment_id
 *
 * (\** This is not an option because I will add other types later when we decide what to do
 *     about symbolic array indexing *\)
 * and offset =
 *   | Const of int (\** Constant offset *\)
 *   | Unknown
 *
 * let cint = {
 *   unqualified = Cint {name = "int"; signed= true; size = 4; ischar = false };
 *   const = false;
 *   volatile = false;
 *   restrict = false;
 *   constexpr = false;
 * }
 *
 * let rec sizeof_unqualified = function
 *   | Machine i -> i;
 *   | Cint {size;_} -> size
 *   | Cbool -> 1
 *   | Ptr _ -> ptrsize
 *   | Struct _ -> assert(false)
 *   | Array _ -> assert(false)
 *   | Enum _ -> sizeof_unqualified cint.unqualified
 *
 * let sizeof t = sizeof_unqualified t.unqualified
 *
 * module Struct = struct
 *
 *   module Field = struct
 *     type t = {fname : string option; offset : int; typ : t; size : int}
 *
 *     let len t = t.size
 *   end
 *
 *   module FieldMap = RngMap.Make(Field)
 *
 *   type t = {layout :FieldMap.t; name : string option}
 *
 *   module Env = struct
 *     type struc = t
 *
 *     type t = {layouts: struc Vector.t; names : string Bimap.t}
 *   end
 * end
 *
 *
 * type enum = {name: string option; labels: (int, string) HashMap.t }
 *
 * type enum_env = enum vector *)

type cinteger = { name : string; signed : bool; size : int }

type cchar = { name : string; signed : bool }

(* void is not a valid type in this system. void* is Unknown* which *)
type base =
  | Unknown
  | Cint of cinteger
  | Cchar of cchar
  | Cbool
  | Ptr of ptr
  (* | Struct of structure *)
  | Array of arr

and arr_dim = { count : int option }

and arr = { elem : t; dims : arr_dim list }

and t = { base : base; const : bool; volatile : bool; restrict : bool }

and ptr = { target : t }

and field = { offset : int; typ : t }

(** TODO add some unique identifier to avoid recursive analysis. The name for now*)
and structure = { name : string; size : int; fields : field SMap.t }

let base b = { base = b; const = false; volatile = false; restrict = false }

let cint : t =
  {
    base = Cint { name = "int"; signed = true; size = 4 };
    const = false;
    volatile = false;
    restrict = false;
  }

let cuchar : t =
  {
    base = Cchar { name = "unsigned char"; signed = false };
    const = false;
    volatile = false;
    restrict = false;
  }

let unknown = base Unknown

type linksem_t = Dwarf.c_type

let base_type_of_linksem name encoding size =
  let encoding = Z.to_int encoding in
  if encoding = vDW_ATE_boolean then Cbool
  else if encoding = vDW_ATE_signed || encoding = vDW_ATE_unsigned then
    match size with
    | Some s -> Cint { name; signed = encoding = vDW_ATE_signed; size = Z.to_int s }
    | None ->
        failwith
        @@ Printf.sprintf "In Ctype.base_of_linksem: integer type %s do not have a size" name
  else if encoding = vDW_ATE_signed_char || encoding = vDW_ATE_unsigned_char then
    Cchar { name; signed = encoding = vDW_ATE_signed_char }
  else failwith @@ Printf.sprintf "In Ctype.base_of_linksem: encoding %x unknown" encoding

let of_linksem (ltyp : linksem_t) : t =
  let rec ol_without_qualifiers : linksem_t -> base = function
    | CT (CT_base (cupdie, name, encoding, size)) -> base_type_of_linksem name encoding size
    | CT (CT_pointer (_, Some t)) -> Ptr { target = ol t }
    | CT (CT_pointer (_, None)) -> Ptr { target = unknown }
    | CT (CT_array (_, elem, l)) -> Array { elem = ol elem; dims = List.map arr_dim_ol l }
    | _ -> Unknown
  and arr_dim_ol (count, typ) = { count = Option.map Z.to_int count }
  and ol ?typedef ?(const = false) ?(volatile = false) ?(restrict = false) : linksem_t -> t =
    function
    | CT (CT_const (_, Some t)) -> ol ~const:true ~volatile ~restrict t
    | CT (CT_const (_, None)) -> { base = Unknown; const = true; volatile; restrict }
    | CT (CT_volatile (_, t)) -> ol ~const ~volatile:true ~restrict t
    | CT (CT_restrict (_, t)) -> ol ~const ~volatile ~restrict:true t
    | CT (CT_typedef (_, name, t, _)) -> ol ~typedef:name ~const ~volatile ~restrict t
    | t ->
        let base = ol_without_qualifiers t in
        { base; const; volatile; restrict }
  in
  ol ltyp

let pp_signed signed = PP.(if signed then !^"s" else !^"u")

let rec pp typ =
  PP.(
    let pp_base = function
      | Cint { name; signed; size } ->
          !^name ^^ lbrace ^^ pp_signed signed ^^ int (8 * size) ^^ rbrace
      | Cbool -> !^"bool"
      | Cchar { name; signed } -> !^name ^^ lbrace ^^ pp_signed signed ^^ rbrace
      | Ptr { target; _ } -> surround 2 0 lparen (pp target) rparen ^^ star
      | Array arr -> pp_arr arr
      | Unknown -> !^"unknown"
    in
    let const = if typ.const then !^" const" else empty in
    let volatile = if typ.volatile then !^" volatile" else empty in
    let restrict = if typ.restrict then !^" restrict" else empty in
    pp_base typ.base ^^ const ^^ volatile ^^ restrict)

and pp_arr arr =
  PP.(surround 2 0 lparen (pp arr.elem) rparen ^^ (List.map pp_arr_dim arr.dims |> concat))

and pp_arr_dim dim = PP.(lbracket ^^ Option.fold ~none:empty ~some:int dim.count ^^ rbracket)
