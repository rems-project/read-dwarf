(** This module provide high-level support for a symbolic array of bytes.

    The main differences between this and a big [concat] expression is that:
    - It allows some bytes to be undefined
    - It allows fast (logarithmic or better) access to any byte or group of bytes.
    - It provides dedicated functions for efficiently extracting an expression
        from a range of bytes (reading) or blitting (writing) an expression to a
        range of bytes.
    - The indexing can start at any point (negative value can be mapped) without any costs

    This data structure deliberately do not have any infrastructure to read or write
    bytes at symbolic positions. See {!SymbolicFragment} for that.

    This data structure do not have a concept of beginning or an end
    (But it has the concept of first defined byte and last defined byte).
    In particular addresses can be negative.

    It is functorized of the type of variables ({!Var}) to get variable equality
    and pretty-printing

    Currently this has a pure interface.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** This is the type signature for variable required by this module *)
module type Var = sig
  (** The type of variables *)
  type t

  (** Equality predicate that will be passed to expressions *)
  val equal : t -> t -> bool

  (** Pretty printer to be used, both for memory pretty printing and for sending memory to Z3 *)
  val pp : t -> PP.document
end

module type S = sig
  (** The type of variables used *)
  type var

  (** The module for manipulated expressions *)
  module Exp : sig
    (** The type of expression in the fragment *)
    type t = (Ast.lrng, var, Ast.no, Ast.no) Ast.exp

    (** Equality on expressions derived from the provided equality on variable {!Var.equal} *)
    val equal : t -> t -> bool

    (** Pretty print the expression in human readable format. See module {!PPExp} *)
    val pp : t -> PP.document

    (** Pretty print the expression in a SMTLIB format *)
    val pp_smt : t -> PP.document
  end

  (** The type of symbolic bytes. *)
  type t

  (** The empty symbolic byte sequence with no bytes defined *)
  val empty : t

  (** Extract an expression in [\[pos:pos+len)]. If any bytes in the range is undefined,
      Then [None] is returned, otherwise a expression is returned *)
  val sub : pos:int -> len:int -> t -> Exp.t option

  (** Write the expresion on the interval [\[pos:pos +len)] of the bytes.
      The expression must be a bitvector of size exactly [8 * len].

      TODO check it when values are type annotated *)
  val blit_exp : Exp.t -> pos:int -> len:int -> t -> t

  (** Clear a range of the symbolic bytes, making all those bytes undefined again.
      If a bound is missing, it means up to infinity in that direction *)
  val clear_bounds : ?start:int -> ?endp:int -> t -> t

  (** Map a function over all the contained expressions *)
  val map_exp : (Exp.t -> Exp.t) -> t -> t

  (** Iter a function over all the contained expressions *)
  val iter_exp : (Exp.t -> unit) -> t -> unit

  (** Pretty prints the symbolic bytes *)
  val pp : t -> PP.document
end

(*****************************************************************************)
(*        Implementation                                                     *)
(*****************************************************************************)

module Make (Var : Var) : S with type var = Var.t = struct
  type var = Var.t

  module Exp = struct
    type t = (Ast.lrng, Var.t, Ast.no, Ast.no) Ast.exp

    let equal (e : t) (e' : t) = Ast.equal_exp ~var:Var.equal e e'

    let pp (exp : t) = PPExp.pp_exp (fun v -> PP.(bar ^^ Var.pp v ^^ bar)) exp

    let pp_smt (exp : t) = Ast.pp_exp Var.pp (AstManip.allow_lets @@ AstManip.allow_mem exp)
  end

  include RngMap.Make (RngMap.PairLenObject (Exp))

  (* Crop a RngMap.PairLenObject(Exp) *)
  let crop ~pos ~len (e, elen) =
    assert (len > 0);
    assert (pos >= 0);
    assert (pos + len <= elen);
    (Ast.Op.extract (8 * (pos + len)) (8 * pos) e, len)

  (* Warning: This code is complicated because of all the indices. I tried to make diagrams
     to explain *)
  let sub ~pos ~len sb =
    assert (len > 0);
    let open Opt in
    let rec sub_list ~pos ~len sb =
      let* ((e, elen), off) = at_off_opt sb pos in
      (* fast case where we ask for exactly one object *)
      if off = 0 && elen = len then Some [e]
      else
        (* start = the start of the object i.e pos - off *)
        let off_len = elen - off in
        if off_len < len then
          (*
             | ------- | ------- | -------- |
           start      pos      next
             |<--off-->|
                       |<-------len-------->|
             |<------elen------->|
                       |<off_len>|
             *)
          let next = pos + off_len in
          let* rest = sub_list ~pos:next ~len:(len - next) sb in
          let nexp = Ast.Op.extract (8 * elen) (8 * off) e in
          Some (nexp :: rest)
        else
          (* off_len >= len *)
          (*
             | ------- | ------- | -------- |
           start      pos
             |<--off-->|
                       |<--len-->|
             |<-----------elen------------->|
                       |<-----off_len------>|
             |<----taken_len---->|
             *)
          let taken_len = off + len in
          Some [Ast.Op.extract (8 * taken_len) (8 * off) e]
    in
    let+ list = sub_list ~pos ~len sb in
    Ast.Op.concat list

  let blit_exp exp ~pos ~len sb =
    assert (len > 0);
    assert (len < Int.max_int / 8);
    let sb = clear_crop sb ~pos ~len ~crop in
    add sb pos (exp, len)

  let map_exp f sb = map (Pair.map f Fun.id) sb

  let iter_exp f sb = iter (Pair.iter f ignore) sb

  let pp sb =
    sb |> bindings
    |> List.map
         PP.(Pair.map shex (fun (exp, len) -> Exp.pp exp ^^ dprintf " of %d bits" (len * 8)))
    |> PP.mapping ""
end
