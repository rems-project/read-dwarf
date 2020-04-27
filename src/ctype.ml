(** This module represent ctypes as they are represented internally.

*)

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

(** This is to represent a naming scope for structs *)
type 'a named_env = (string, 'a) IdMap.t

(** The unqualified part of the C type without const volatile, ...*)
type unqualified =
  | Machine of int  (** Size in bytes for now*)
  | Cint of { name : string; signed : bool; size : int; ischar : bool }
  | Cbool
  | Ptr of { fragment : fragment; offset : offset }
  | Struct of { name : string; size : int; id : int }
      (** See {!env} for what the id refers to. The int is the size *)
  | Array of { elem : t; dims : int option list }
  | Enum of { name : string; id : int }  (** See {!env} for what the id refers to *)

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
  | FreeFragment of int  (** Writable fragment type for memory whose type is changing *)

(** The type of an offset in a fragment *)
and offset = Const of int  (** Constant offset *) | Somewhere

(** The type of C types in Linksem *)
type linksem_t = Dwarf.c_type

(** A field in a structure *)
type field = { fname : string option; offset : int; typ : t; size : int }

type linksem_field = linksem_t Dwarf.struct_union_member

(** A range map over field to represent a structure layout  *)
module FieldMap : RngMap.S with type obj = field = RngMap.Make (struct
  type t = field

  let len f = f.size
end)

(** The type of a C structure.

    A structure can be complete or incomplete but due to some internal hackery this is a
    need for a subtle difference with C: Even incomplete structure have a size,
    they just don't have any field.

    A incomplete struct can and will often be complete later as the interpretation
    of DWARF information advances.

    The name field is the linking name of the struct.
*)
type struc = { layout : FieldMap.t; name : string; size : int; complete : bool }

(** The type of a C enumeration *)
type enum = { name : string; labels : (int, string option) Hashtbl.t }

(** The identifier for a linksem_cupdie. See {!ids_of_cupdie} *)
type cupdie_id = int * int

(** The type of environement linksem gives us.

    Only structs, enums and unions can appear. A type can appear multiple times
    and also be forward-declared with missing data like size.
    {!env_of_linksem} hopefully deals with all those problems
*)
type linksem_env = linksem_t list

(** An versiont of the linksem environement indexed by {!cupdie_id} *)
type linksem_indexed_env = (cupdie_id, linksem_t) Hashtbl.t

(** The type environment that contain mapping from linking name and a generated id

    to the actual content of structs and enumerations.

    Linking names can be:
      - A plain name for a struct/enum declared with a tag.
      - [typedef.name] for an unnamed struct declared in a typedef
      - [outer.member] for unnamed struct used as the type of a [member]
        of a struct with linking name [outer].

    As an unnamed struct can be declared in the linksem environement but
    used with the typedef only after the initial environement setup of
    {!env_of_linksem}, the original linksem type must be kept alive in
    [lenv]
*)
type env = { structs : struc named_env; enums : enum named_env; lenv : linksem_indexed_env }

(** The size of pointer. This will be configurable later *)
let ptr_size = 8

(** The size of enums. This will be configurable later *)
let enum_size = 4

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Convenience manipulation } *)

let is_struct t = match t.unqualified with Struct _ -> true | _ -> false

let is_array t = match t.unqualified with Array _ -> true | _ -> false

let is_ptr t = match t.unqualified with Ptr _ -> true | _ -> false

let is_scalar t =
  match t.unqualified with
  | Machine _ -> true
  | Cint _ -> true
  | Cbool -> true
  | Ptr _ -> true
  | Enum _ -> true
  | _ -> false

let is_composite t = match t.unqualified with Struct _ -> true | Array _ -> true | _ -> false

let is_constexpr t = t.constexpr

(** Make a simple pointer from a type *)
let ptr t = Ptr { fragment = DynArray t; offset = Const 0 }

(** A void* pointer *)
let voidstar = Ptr { fragment = Unknown; offset = Somewhere }

(** Create a qualified type from an unqualified type with the specified qualifiers *)
let qual ?(const = false) ?(volatile = false) ?(restrict = false) ?(constexpr = false) unqualified
    =
  { unqualified; const; volatile; restrict; constexpr }

(** update specified qualifiers. Other qualifier are kept *)
let add_qual ?const ?volatile ?restrict ?constexpr old =
  let unqualified = old.unqualified in
  let const = Option.value const ~default:old.const in
  let volatile = Option.value volatile ~default:old.volatile in
  let restrict = Option.value restrict ~default:old.restrict in
  let constexpr = Option.value constexpr ~default:old.constexpr in
  { unqualified; const; volatile; restrict; constexpr }

(** Create a machine type of that size without qualifiers *)
let machine ?(constexpr = false) size = Machine size |> qual ~constexpr

(** Create a pointer to fragment with specified offset (0 by default) *)
let of_frag ?(offset = 0) ?(restrict = false) fragment =
  Ptr { fragment; offset = Const offset } |> qual ~restrict

(** Build an incomplete struct with a linking name and a size *)
let incomplete_struct name size = { layout = FieldMap.empty; name; size; complete = false }

(** Makes an empty environement from and indexed linksem environement *)
let make_env lenv = { structs = IdMap.make (); enums = IdMap.make (); lenv }

(** Update an {!offset} with an integer update *)
let offset_update offset update =
  match offset with Const i -> Const (i + update) | Somewhere -> offset

(** Update an pointer with an integer update *)
let ptr_update ptr update =
  match ptr.unqualified with
  | Ptr { fragment; offset } ->
      { ptr with unqualified = Ptr { fragment; offset = offset_update offset update } }
  | _ -> Raise.inv_arg "ptr_update: not a pointer"

(** Make a pointer forget it's offset *)
let ptr_forget ptr =
  match ptr.unqualified with
  | Ptr { fragment; offset } -> { ptr with unqualified = Ptr { fragment; offset = Somewhere } }
  | _ -> Raise.inv_arg "ptr_forget: not a pointer"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Sizeof }

    This section give implementation of sizeof function.

    Dynamic array have size 0
    until we are able to deal with C99 last member dynamic arrays.
*)

(** Give the size of an {!unqualified} type. Need the environement. *)
let rec sizeof_unqualified = function
  | Machine i -> i
  | Cint { size; _ } -> size
  | Cbool -> 1
  | Ptr _ -> ptr_size
  | Struct { size; _ } -> size
  | Enum i -> enum_size
  | Array { elem; dims } ->
      let num = dims |> List.map (Option.value ~default:0) |> List.fold_left ( * ) 0 in
      num * sizeof elem

(** Give the size of an type. Need the environement. *)
and sizeof t = sizeof_unqualified t.unqualified

(** For being used in {!RngMap.LenObject} *)
let len = sizeof

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Linksem DWARF to internal Ctype conversion }

    This section contain the whole hierarchy of function used to convert
    type from DWARF representation to the internal type system.

    The top-level interface for types is {!of_linksem} and
    the top-level interface for environement is {!env_of_linksem}

*)

(** This type is a conversion context.
    It role is to contain all the thing that all the function in this section
    will need to convert type.*)
type conversion_context = { env : env; potential_link_name : string option }

(** Get the id of a linksem [cupdie] *)
let ids_of_cupdie ((cu, p, die) : Dwarf.cupdie) : cupdie_id =
  (Z.to_int cu.cu_header.cuh_offset, Z.to_int die.die_offset)

(** Pretty print the dwarf decl type

    TODO: Move that in the appropriate place *)
let pp_decl (d : Dwarf.decl) =
  PP.dprintf "File %s, line %d"
    (Option.value d.decl_file ~default:"?")
    (d.decl_line |> Option.map Z.to_int |> Option.value ~default:0)

(** This exception is raised when the type we are trying to reach
    must came from another translation unit or later in the current one.

    The information is incomplete at this moment to create is.

    Normally this exception should only happen during the initial
    {!env_of_linksem}. If it happens elsewhere, either the code used
    an anonymous struct that do not have C++-like linkage or the
    compiler did not do it's job.
*)
exception LinkError

let expect_some_link = Opt.value_fun ~default:(fun _ -> raise LinkError)

(** Convert a base type with a name and encoding and maybe a size to its
    inner representation

    Only integers, chars and bools supported. No floating points
*)
let base_type_of_linksem ?size ~encoding name =
  let encoding = Z.to_int encoding in
  if encoding = vDW_ATE_boolean then Cbool
  else if encoding = vDW_ATE_signed || encoding = vDW_ATE_unsigned then
    let size =
      Opt.value_fail size "In Ctype.base_type_of_linksem: integer type %s do not have a size" name
    in
    Cint { name; signed = encoding = vDW_ATE_signed; size = Z.to_int size; ischar = false }
  else if encoding = vDW_ATE_signed_char || encoding = vDW_ATE_unsigned_char then
    Cint { name; signed = encoding = vDW_ATE_signed_char; size = 1; ischar = true }
  else Raise.fail "In Ctype.base_of_linksem: encoding %x unknown" encoding

let rec field_of_linksem ~cc ((_, fname, ltyp, offset) : linksem_field) : field =
  let newpln =
    Opt.(
      let+ pln = cc.potential_link_name and+ fname = fname in
      String.concat "." [pln; fname])
  in
  debug "Processing field %t" PP.(top (opt string) fname);
  let cc = { cc with potential_link_name = newpln } in
  let typ = of_linksem_cc ~cc ltyp in
  debug "Processing sizeof field %t" PP.(top (opt string) fname);
  let size = sizeof typ in
  debug "Processed field %t" PP.(top (opt string) fname);
  let offset = Z.to_int offset in
  { fname; offset; typ; size }

and field_map_of_linksem ~cc l : FieldMap.t =
  List.fold_left
    (fun m f ->
      let f = field_of_linksem ~cc f in
      FieldMap.add m f.offset f)
    FieldMap.empty l

and struc_of_linksem ~cc name size members : struc =
  let layout = field_map_of_linksem ~cc members in
  { name; size; layout; complete = true }

(** Build a struct from it's cupdie and name.
    If [force_complete] is true and the struct is incomplete. It will try
    to complete is using [cupdie] and throw {!LinkError} if it fails.
*)
and struct_type_of_linksem ?(force_complete = false) ~cc ~cupdie ~mname ~decl : unqualified =
  let open Dwarf in
  (* If the struct has no name we fallback on the potential name. If the
     the fallback fails, we throw a link error *)
  let name = Opt.(expect_some_link (mname ||| cc.potential_link_name)) in
  debug "Processing struct %s" name;
  match IdMap.to_ident_opt cc.env.structs name with
  | Some id when not force_complete ->
      Struct { name; id; size = (IdMap.geti cc.env.structs id).size }
  | Some id -> (
      (* If force_complete and struct is defined, check if it is complete *)
      let struc = IdMap.geti cc.env.structs id in
      if struc.complete then Struct { name; id; size = struc.size }
      else
        (* Incomplete struct: try to complete it *)
        match Hashtbl.find cc.env.lenv (ids_of_cupdie cupdie) with
        | CT (CT_struct_union (_, Atk_structure, _, msize, _, Some members)) ->
            let lsize = expect_some_link msize in
            let size = Z.to_int lsize in
            let cc = { cc with potential_link_name = Some name } in
            let struc : struc = struc_of_linksem ~cc name size members in
            IdMap.seti cc.env.structs id struc;
            Struct { name; id; size }
        | _ ->
            Raise.fail "%t: DWARF type environement corrupted: struct %s is not a struct"
              PP.(tos pp_decl decl)
              name
    )
  | None -> (
      (* If the struct is not defined, try to define it *)
      match Hashtbl.find cc.env.lenv (ids_of_cupdie cupdie) with
      | CT (CT_struct_union (_, Atk_structure, _, msize, _, Some members)) ->
          let lsize = expect_some_link msize in
          let size = Z.to_int lsize in
          let id = IdMap.add cc.env.structs name @@ incomplete_struct name size in
          let cc = { cc with potential_link_name = Some name } in
          let struc : struc = struc_of_linksem ~cc name size members in
          IdMap.seti cc.env.structs id struc;
          Struct { name; id; size }
      | _ ->
          Raise.fail "%t: DWARF type environement corrupted: struct %s is not a struct"
            PP.(tos pp_decl decl)
            name
    )

and enum_of_linksem ~cc name llabels : enum =
  let labels = Hashtbl.create 5 in
  List.iter (fun (_, name, value) -> Hashtbl.add labels (Z.to_int value) name) llabels;
  { name; labels }

and enum_type_of_linksem ~cc ~cupdie ~mname ~decl : unqualified =
  let open Dwarf in
  (* If the enum has no name we fallback on the potential name *)
  let name = Opt.(expect_some_link (mname ||| cc.potential_link_name)) in
  match IdMap.to_ident_opt cc.env.enums name with
  | Some id -> Enum { name; id }
  | None -> (
      match Hashtbl.find cc.env.lenv (ids_of_cupdie cupdie) with
      | CT (CT_enumeration (_, _, _, _, _, Some labels)) ->
          let enum : enum = enum_of_linksem ~cc name labels in
          Enum { name; id = IdMap.add cc.env.enums name enum }
      | _ ->
          Raise.fail "%t: DWARF type environement corrupted: enum %s is not a enum"
            PP.(tos pp_decl decl)
            name
    )

(** Convert an unqualified type. Union are just [Machine n] where n is their size *)
and unqualified_of_linksem ?(force_complete = false) ~cc : linksem_t -> unqualified = function
  | CT (CT_base (cupdie, name, encoding, size)) -> base_type_of_linksem ~encoding ?size name
  | CT (CT_pointer (_, Some t)) -> ptr @@ of_linksem_cc ~cc t
  | CT (CT_pointer (_, None)) -> voidstar
  | CT (CT_array (_, elem, l)) ->
      Array { elem = of_linksem_cc ~cc elem; dims = List.map Fun.(fst %> Option.map Z.to_int) l }
  | CT (CT_struct_union (cupdie, Atk_structure, mname, _, decl, _)) ->
      struct_type_of_linksem ~force_complete ~cc ~cupdie ~mname ~decl
  | CT (CT_struct_union (_, Atk_union, name, size, decl, _)) ->
      let size =
        match size with
        | Some s -> Z.to_int s
        | None ->
            warn "%t: Sizeless union defaulting to 8 for now" PP.(top pp_decl decl);
            8
      in
      Machine size
  | CT (CT_enumeration (cupdie, mname, _, _, decl, _)) ->
      enum_type_of_linksem ~cc ~cupdie ~mname ~decl
  | _ -> Raise.inv_arg "Converting qualified type in unqualified"

(** The main [of_linksem] that take a full conversion_context and qualifiers.

    The user friendly version is {!of_linksem}

    See {!struct_type_of_linksem} for the explanation of [force_complete].

    All the qualifier passed as parameter are added to the resulting type.
*)
and of_linksem_cc ?(force_complete = false) ~cc ?(const = false) ?(volatile = false)
    ?(restrict = false) : linksem_t -> t = function
  | CT (CT_const (_, Some t)) -> of_linksem_cc ~cc ~const:true ~volatile ~restrict t
  | CT (CT_const (_, None)) ->
      { unqualified = Machine 0; const = true; volatile; restrict; constexpr = false }
  | CT (CT_volatile (_, t)) -> of_linksem_cc ~cc ~const ~volatile:true ~restrict t
  | CT (CT_restrict (_, t)) -> of_linksem_cc ~cc ~const ~volatile ~restrict:true t
  | CT (CT_typedef (_, name, t, _)) ->
      let cc = { cc with potential_link_name = Some ("typedef." ^ name) } in
      of_linksem_cc ~cc ~const ~volatile ~restrict t
  | t ->
      let unqualified = unqualified_of_linksem ~force_complete ~cc t in
      { unqualified; const; volatile; restrict; constexpr = false }

(** The user friendly interface that convert a type using the environment. This useful
    when there is no additional linking information to pass on *)
let of_linksem ~env (ltyp : linksem_t) : t =
  let cc = { env; potential_link_name = None } in
  of_linksem_cc ~cc ltyp

(** The main environment conversion function.

    This the function that deal with all the type linking process,
    forward declaration an all that stuff.

    First it create the indexed linksem environment (member lenv of {!env}),

    Then it register all named structs as incomplete in the environment
    (to deal with self recursion, only named structs can self-recurse).

    Finally it run {!of_linksem_cc} on all the type with [force_complete] on.
    During this phase it ignore all {!LinkError} that arise. It assumes that
    if some thing was incomplete at that point, it will be completed later.

    Then it can return the freshly built environment.
*)
let env_of_linksem (lenv : linksem_env) : env =
  let open Dwarf in
  (* First phase: index them by the cupdie number id *)
  let llenv : linksem_indexed_env = Hashtbl.create 10 in
  List.iter
    (function
      | CT (CT_struct_union (cupdie, _, _, _, _, _)) as ct ->
          Hashtbl.add llenv (ids_of_cupdie cupdie) ct
      | CT (CT_enumeration (cupdie, _, _, _, _, _)) as ct ->
          Hashtbl.add llenv (ids_of_cupdie cupdie) ct
      | _ -> ())
    lenv;
  (* Second phase: Predeclare all named struct as incomplete *)
  let env = make_env llenv in
  List.iter
    (function
      | CT (CT_struct_union (cupdie, Atk_structure, mname, msize, _, _)) as ct ->
          Opt.(
            let+! name = mname and+ size = msize in
            if not @@ IdMap.mem env.structs name then
              IdMap.add env.structs name @@ incomplete_struct name (Z.to_int size) |> ignore)
      | _ -> ())
    lenv;
  (* Third phase: Add all the type to the result environement *)
  let cc : conversion_context = { env; potential_link_name = None } in
  List.iter
    (fun lt -> try of_linksem_cc ~force_complete:true ~cc lt |> ignore with LinkError -> ())
    lenv;
  cc.env

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

open PP

let pp_signed signed = if signed then !^"s" else !^"u"

(** Pretty print a type. If an environement is provided, structs and enums
    will be printed with a name, otherwise they will just have a number *)
let rec pp typ =
  let const = if typ.const then !^"const " else empty in
  let volatile = if typ.volatile then !^"volatile " else empty in
  let restrict = if typ.restrict then !^"restrict " else empty in
  let constexpr = if typ.constexpr then !^"constexpr " else empty in
  const ^^ volatile ^^ restrict ^^ constexpr ^^ pp_unqualified typ.unqualified

and pp_unqualified = function
  | Machine i -> dprintf "M%d" i
  | Cint { name; signed; size; ischar } ->
      !^name ^^ lparen ^^ pp_signed signed ^^ int (8 * size) ^^ rparen
  | Cbool -> !^"bool"
  | Ptr { fragment; offset } ->
      surround 2 0 lbrace (pp_offset offset ^^ pp_fragment fragment) rbrace
  | Array { elem; dims } -> pp_arr elem dims
  | Struct { name; _ } -> dprintf "Struct %s" name
  | Enum { name; _ } -> dprintf "Enum %s" name

and pp_fragment frag =
  group
  @@
  match frag with
  | Single t -> pp t
  | DynArray t -> pp t ^^ !^"[]"
  | Unknown -> !^"unknown"
  | FreeFragment i -> dprintf "frag %d" i

and pp_offset = function
  | Const off when off = 0 -> empty
  | Const off -> !^"at " ^^ shex off ^^ !^" in" ^^ space
  | Somewhere -> !^"somewhere in" ^^ space

and pp_arr elem dims =
  PP.(surround 2 0 lparen (pp elem) rparen ^^ (List.map pp_arr_dim dims |> concat))

and pp_arr_dim dim = PP.(lbracket ^^ Option.fold ~none:empty ~some:int dim ^^ rbracket)

let pp_field ?env { fname; offset; typ; size } =
  let name = Option.value fname ~default:"_" in
  infix 2 1 $ colon $ !^name $ pp typ

let pp_struct { layout; name; size; complete } =
  let fields = FieldMap.bindings layout |> List.map (Pair.map PP.ptr @@ pp_field) in
  PP.(mapping (if complete then name else name ^ "?") (fields @ [(PP.ptr size, !^"end")]))

let pp_enums { name; labels } = PP.(hashtbl ptr (Opt.fold ~none:underscore ~some:string) labels)

(** Print the whole environement (not the linksem indexed environment) *)
let pp_env env =
  PP.(
    record "env"
      [
        ("structs", IdMap.pp ~keys:string ~vals:pp_struct env.structs);
        ("enums", IdMap.pp ~keys:string ~vals:pp_enums env.enums);
      ])

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Type at }

    This section is about getting the type at a specific offset in things.
*)

(** Same as {!type_at} but for structs *)
let rec struct_at ~env ~size struc at : t option =
  let open Opt in
  let* (field, off) = FieldMap.at_off_opt struc.layout at in
  type_at ~env ~size field.typ off

(** Get the type of size [size] at the provided offset in another type*)
and type_at ~env ~size typ at : t option =
  let open Opt in
  if at = 0 && size >= sizeof typ then begin
    if size > sizeof typ then warn "type_at: size %d in only %t" size (PP.top pp typ);
    Some typ
  end
  else
    let+ rtyp =
      match typ.unqualified with
      | Machine n ->
          if at + size > n then warn "type_at: machine out of bound access";
          let constexpr = typ.constexpr in
          machine ~constexpr size |> some
      | Struct { id; _ } ->
          let struc = IdMap.geti env.structs id in
          struct_at ~env ~size struc at
      | Array { elem; _ } ->
          let at = at mod sizeof elem in
          type_at ~env ~size elem at
      | _ ->
          warn "type_at: reading inside %t is wierd" (PP.top pp typ);
          None
    in
    let const = Opt.of_bool ~some:true typ.const in
    let volatile = Opt.of_bool ~some:true typ.volatile in
    add_qual ?const ?volatile rtyp
