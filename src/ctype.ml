(** This module represent ctypes as they are represented internally.

*)

module SMap = Map.Make (String)

open Logs.Logger (struct
  let str = "Ctype"
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 DWARF constants } *)

let vDW_ATE_address = "DW_ATE_address" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_boolean = "DW_ATE_boolean" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_signed = "DW_ATE_signed" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_signed_char = "DW_ATE_signed_char" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_unsigned = "DW_ATE_unsigned" |> Dwarf.base_type_attribute_encode |> Z.to_int

let vDW_ATE_unsigned_char = "DW_ATE_unsigned_char" |> Dwarf.base_type_attribute_encode |> Z.to_int

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Types } *)

type enum_id = int

type structure_id = int

type fragment_id = int

(** This is to represent a naming scope for structs *)
type 'a named_env = (string, 'a) IdMap.t

(** The unqualified part of the C type without const volatile, ...*)
type unqualified =
  | Machine of int  (** Size in bytes for now*)
  | Cint of { name : string; signed : bool; size : int; ischar : bool }
  | Cbool
  | Ptr of { fragment : fragment; offset : offset }
  | Struct of structure_id  (** See {!env} for what the id refers to *)
  | Array of { elem : t; dims : int option list }
  | Enum of enum_id  (** See {!env} for what the id refers to *)

(** The internal representation of generalized C types *)
and t = {
  unqualified : unqualified;
  const : bool;
  volatile : bool;
  restrict : bool;
  constexpr : bool;
}

(** The type of a memory fragment *)
and fragment =
  | Unknown  (** Unknown type, But without possibility of learning*)
  | Single of t  (** Single object: Only when accessing of a global variable *)
  | DynArray of t  (** Generic C pointer, may point to multiple element of that type *)
  | FreeFragment of fragment_id  (** Writable fragment type for memory whose type is changing *)

(** The type of an offset in a fragment *)
and offset = Const of int  (** Constant offset *) | Somewhere

(** A field in a structure *)
type field = { fname : string option; offset : int; typ : t; size : int }

(** A range map over field to represent a structure layout  *)
module FieldMap = RngMap.Make (struct
  type t = field

  let len f = f.size
end)

(** The type of a C structure *)
type struc = { layout : FieldMap.t; name : string; size : int }

(** The type of a C enumeration *)
type enum = { name : string; labels : (int, string option) Hashtbl.t }

(** The type environment that contain mapping from name and id to the layout
    of structs and enums *)
type env = { structs : struc named_env; enums : enum named_env }

(** Makes an empty environement *)
let make_env () = { structs = IdMap.make (); enums = IdMap.make () }

(** The type of C types in Linksem *)
type linksem_t = Dwarf.c_type

(** The size of pointer. This will be configurable later *)
let ptr_size = 8

(** The size of enums. This will be configurable later *)
let enum_size = 4

(** Make a simple pointer from a type *)
let ptr t = Ptr { fragment = DynArray t; offset = Const 0 }

(** A void* pointer *)
let voidstar = Ptr { fragment = Unknown; offset = Somewhere }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Sizeof }

    This section give implementation of sizeof function.

    TODO: Do alignof too.
*)

let sizeof_unqualified ~env = function
  | Machine i -> i
  | Cint { size; _ } -> size
  | Cbool -> 1
  | Ptr _ -> ptr_size
  | Struct i -> (IdMap.geti env.structs i).size
  | Enum i -> enum_size
  | Array _ -> failwith "sizof array"

let sizeof t = sizeof_unqualified t.unqualified

let sizeof_fieldmap fm = failwith "unimplemented"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Linksem DWARF to internal Ctype conversion } *)

(** TODO: Move that in the appropriate place *)
let pp_decl (d : Dwarf.decl) =
  PP.dprintf "File %s, line %d"
    (Option.value d.decl_file ~default:"?")
    (d.decl_line |> Option.map Z.to_int |> Option.value ~default:0)

let base_type_of_linksem name encoding size =
  let encoding = Z.to_int encoding in
  if encoding = vDW_ATE_boolean then Cbool
  else if encoding = vDW_ATE_signed || encoding = vDW_ATE_unsigned then
    match size with
    | Some s ->
        Cint { name; signed = encoding = vDW_ATE_signed; size = Z.to_int s; ischar = false }
    | None -> Raise.fail "In Ctype.base_of_linksem: integer type %s do not have a size" name
  else if encoding = vDW_ATE_signed_char || encoding = vDW_ATE_unsigned_char then
    Cint { name; signed = encoding = vDW_ATE_signed_char; size = 1; ischar = true }
  else Raise.fail "In Ctype.base_of_linksem: encoding %x unknown" encoding

let of_linksem ?(env = make_env ()) (ltyp : linksem_t) : t =
  let rec ol_without_qualifiers ?typedef : linksem_t -> unqualified = function
    | CT (CT_base (cupdie, name, encoding, size)) -> base_type_of_linksem name encoding size
    | CT (CT_pointer (_, Some t)) -> ptr @@ of_linksem' t
    | CT (CT_pointer (_, None)) -> voidstar
    | CT (CT_array (_, elem, l)) ->
        Array { elem = of_linksem' elem; dims = List.map Fun.(fst %> Option.map Z.to_int) l }
    | CT (CT_aggregate (_, Atk_structure, name, size, decl, members)) -> (
        (* TODO This should be a separate function *)
        let name =
          match (name, typedef) with
          | (Some n, _) -> n
          | (_, Some t) -> t
          | _ -> Raise.fail "%t Structure without name or typedef" PP.(tos pp_decl decl)
        in
        let layout = field_map_of_linksem members in
        if IdMap.mem env.structs name then
          let id = IdMap.to_ident env.structs name in
          let olayout = (IdMap.geti env.structs id).layout in
          if true (* olayout = layout *) then Struct id
          else Raise.fail "Trying to add struct %s with a different layout that previously" name
        else
          match Option.map Z.to_int size with
          | Some size -> Struct (IdMap.add env.structs name { layout; name; size })
          | None ->
              warn "Struct %s had no size. Defaulting to 8" name;
              Struct (IdMap.add env.structs name { layout; name; size = 8 })
      )
    | CT (CT_aggregate (_, Atk_union, name, size, decl, members)) ->
        let size =
          match size with
          | Some s -> Z.to_int s
          | None ->
              warn "%t: Sizeless union defaulting to 8 for now" PP.(top pp_decl decl);
              8
        in
        Machine size
    | CT (CT_enumeration (_, name, typ, size, decl, members)) ->
        let name =
          match (name, typedef) with
          | (Some n, _) -> n
          | (_, Some t) -> t
          | _ -> Raise.fail "%t: Enumeration without name or typedef" PP.(tos pp_decl decl)
        in
        let size =
          match size with
          | Some s -> Z.to_int s
          | None ->
              warn "Sizeless enumeration %s, defaulting to 4 for now" name;
              4
        in
        let members = enum_of_linksem members in
        if IdMap.mem env.enums name then
          let id = IdMap.to_ident env.enums name in
          let omembers = (IdMap.geti env.structs id).layout in
          if true (* omembers = members *) then Enum id
          else Raise.fail "Trying to add struct %s with a different layout that previously" name
        else Enum (IdMap.add env.enums name { name; labels = members })
    | _ -> Raise.fail "Converting undupported type"
  and enum_of_linksem members =
    let res = Hashtbl.create 5 in
    List.iter (fun (_, name, value) -> Hashtbl.add res (Z.to_int value) name) members;
    res
  and field_of_linksem (_, fname, ltyp, offset) : field =
    let typ = of_linksem' ltyp in
    let size = sizeof ~env typ in
    let offset = Z.to_int offset in
    { fname; offset; typ; size }
  and field_map_of_linksem l : FieldMap.t =
    List.fold_left
      (fun m f ->
        let f = field_of_linksem f in
        FieldMap.add m f.offset f)
      FieldMap.empty l
  and of_linksem' ?typedef ?(const = false) ?(volatile = false) ?(restrict = false) :
      linksem_t -> t = function
    | CT (CT_const (_, Some t)) -> of_linksem' ~const:true ~volatile ~restrict t
    | CT (CT_const (_, None)) ->
        { unqualified = Machine 0; const = true; volatile; restrict; constexpr = false }
    | CT (CT_volatile (_, t)) -> of_linksem' ~const ~volatile:true ~restrict t
    | CT (CT_restrict (_, t)) -> of_linksem' ~const ~volatile ~restrict:true t
    | CT (CT_typedef (_, name, t, _)) -> of_linksem' ~typedef:name ~const ~volatile ~restrict t
    | t ->
        let unqualified = ol_without_qualifiers ?typedef t in
        { unqualified; const; volatile; restrict; constexpr = false }
  in
  of_linksem' ltyp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

open PP

let pp_signed signed = if signed then !^"s" else !^"u"

(** Pretty print a type. If an environement is provided, struct will be printed with a name,
    otherwise they will just have a number *)
let rec pp ?env typ =
  let const = if typ.const then !^"const " else empty in
  let volatile = if typ.volatile then !^"volatile " else empty in
  let restrict = if typ.restrict then !^"restrict " else empty in
  let constexpr = if typ.constexpr then !^"constexpr " else empty in
  const ^^ volatile ^^ restrict ^^ constexpr ^^ pp_unqualified ?env typ.unqualified

and pp_unqualified ?env = function
  | Machine i -> dprintf "M%d" i
  | Cint { name; signed; size; ischar } ->
      !^name ^^ lparen ^^ pp_signed signed ^^ int (8 * size) ^^ rparen
  | Cbool -> !^"bool"
  | Ptr { fragment; offset } ->
      surround 2 0 lbrace (pp_offset offset ^^ pp_fragment ?env fragment) rbrace
  | Array { elem; dims } -> pp_arr ?env elem dims
  | Struct i -> (
      match env with
      | Some env -> dprintf "Struct %s" (IdMap.of_ident env.structs i)
      | None -> dprintf "Struct %d" i
    )
  | Enum i -> (
      match env with
      | Some env -> dprintf "Enum %s" (IdMap.of_ident env.enums i)
      | None -> dprintf "Enum %d" i
    )

and pp_fragment ?env frag =
  group
  @@
  match frag with
  | Single t -> pp ?env t
  | DynArray t -> pp ?env t ^^ !^"[]"
  | Unknown -> !^"unknown"
  | FreeFragment i -> dprintf "frag %d" i

and pp_offset = function
  | Const off when off = 0 -> empty
  | Const off -> dprintf "at 0x%x in" off ^^ space
  | Somewhere -> !^"somewhere in" ^^ space

and pp_arr ?env elem dims =
  PP.(surround 2 0 lparen (pp ?env elem) rparen ^^ (List.map pp_arr_dim dims |> concat))

and pp_arr_dim dim = PP.(lbracket ^^ Option.fold ~none:empty ~some:int dim ^^ rbracket)

let pp_field { fname; offset; typ; size } =
  let name = Option.value fname ~default:"_" in
  infix 2 1 $ !^name $ colon $ pp typ

let pp_struct { layout; name; size } =
  let fields = FieldMap.bindings layout |> List.map (Pair.map PP.int pp_field) in
  PP.(mapping name (fields @ [(PP.ptr size, !^"end")]))

let pp_enums { name; labels } = PP.(hashtbl ptr (opt string) labels)

let pp_env env =
  PP.(
    record "env"
      [
        ("structs", IdMap.pp ~keys:string ~vals:pp_struct env.structs);
        ("enums", IdMap.pp ~keys:string ~vals:pp_enums env.enums);
      ])
