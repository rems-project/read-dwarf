(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module provides a representation of symbolic memory as both a trace and a caching mechanism
    That is able to fetch some actual read value when there is no risk of aliasing.

    A given fragment may not alias with any other fragment.

    This is not to be confused with a type fragment in {!Fragment} (soon to be renamed).

    Support for escaping is not currently present. Escaping must be detected out of this module.

    This module is parameterized by a module for variable.

    The symbolic memory fragment have a immutable interface.

    There is no mli file as the signature is defined by {!S}.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** The signature of a memory fragment module *)
module type S = sig
  (** The type of variables used *)
  type var

  module Size = Ast.Size

  (** The type of expressions stored in the fragment *)
  type exp = (var, Ast.no) Exp.Typed.t

  (** This module provide the concept of memory block, as used by a fragment.

      The block represent a memory area that contain a single memory expression.

      It may represent a symbolic or concrete address. In the fist case,
      It may also be concretely bounded.*)
  module Block : sig
    (** The type for representing memory blocks *)
    type t = private {
      base : exp option;  (** The symbolic base. If [None] the block is concrete *)
      offset : int;  (** The concrete part of the address *)
      size : Size.t;
      bounds : (int * int) option;  (** Optional bounds: [(min, max)] means [\[min:max)] *)
    }

    (** Make the block from an address and a size. The address is automatically split
        between a symbolic and concrete part.*)
    val make_split : ?bounds:int * int -> exp -> Size.t -> t
  end

  (** This module provide the trace of reads and writes to a symbolic fragment. *)
  module Event : sig
    (** Types of memory events *)
    type t =
      | Read of Block.t * var  (** From [Block.t], read [var] *)
      | Write of Block.t * exp  (** To [Block.t], write [exp] *)

    val pp : t -> Pp.document
  end

  (** The type of a memory fragment *)
  type t

  (** Get the trace of given fragment *)
  val get_trace : t -> Event.t list

  (** The empty memory fragment. Any read will be symbolic *)
  val empty : t

  (** Start a new memory fragment based on the previous one *)
  val from : t -> t

  (** Try to read a expression in a block. If one can provide a symbolic expression
      representing the content of the block then [Some] is returned,
      otherwise [None] is returned.*)
  val try_read : t -> Block.t -> exp option

  (** Same semantic as {!try_read} but ignores the caches. Is supposed to be slower.

      The required property is that if {!try_read} return [Some value]
      then [try_read_naive] must return the same value.
      It's possible that try_read_naive give a result when {!try_read} don't
  *)
  val try_read_naive : t -> Block.t -> exp option

  (** Read a symbolic variable from a block. This bound this symbolic variable to the
      The content of the block in the current memory state.contents
  *)
  val read_sym : t -> Block.t -> var -> t

  (** Write a symbolic expression at a block *)
  val write : t -> Block.t -> exp -> t

  (** Map a function over all contained expressions. This function
      must not change the semantic meaning of symbolic expressions in any way.
      It is intended to be used with simplifying functions and the like *)
  val map_exp : (exp -> exp) -> t -> t

  (** Iter a function over all contained expression. Expression may appear more or less
      than anticipated because of various caching. *)
  val iter_exp : (exp -> unit) -> t -> unit

  (** Check cache integrity. Throw if something is wrong *)
  val check_cache : t -> unit

  (** Tells if the memory is empty since it's initial base. (No new memory operation were added *)
  val is_empty : t -> bool

  (** Pretty prints the raw internals fragment. TODO: A nice pretty printer *)
  val pp_raw : t -> Pp.document
end

(*****************************************************************************)
(*        Implementation                                                     *)
(*****************************************************************************)

(** A functor to make a memory fragment module from a variable module {!Var} *)
module Make (Var : Exp.Var) : S with type var = Var.t = struct
  type var = Var.t

  module CCache = SymbolicBytes.Make (Var)
  module Size = Ast.Size
  module Sums = Exp.Sums
  module Exp = Exp.Make (Var)

  type exp = Exp.t

  module Block = struct
    type t = { base : Exp.t option; offset : int; size : Size.t; bounds : (int * int) option }

    let make_split ?bounds addr size : t =
      let (base, bvoffset) = Sums.split_concrete addr in
      assert (not @@ Option.exists Sums.has_concrete_term base);
      let offset = BitVec.to_int bvoffset in
      { base; offset; size; bounds }

    let map_exp (f : Exp.t -> Exp.t) (b : t) : t = { b with base = Option.map f b.base }

    let iter_exp (f : Exp.t -> unit) (b : t) : unit = Option.iter f b.base

    let pp_addr b =
      match b.base with
      | Some base -> Pp.(infix 2 1 plus (Exp.pp base) (shex b.offset))
      | None -> Pp.shex b.offset

    let pp b = Pp.(dprintf "%d bits at " (Size.to_bits b.size) ^^ pp_addr b)
  end

  module Event = struct
    type t = Read of Block.t * var | Write of Block.t * Exp.t

    (* let equal e e' =
     *   match (e, e') with
     *   | (Read (mb, v), Read (mb', v')) -> Var.equal v v' && Block.equal mb mb'
     *   | (Write (mb, e), Write (mb', e')) -> Exp.equal e e' && Block.equal mb mb'
     *   | _ -> false *)

    let map_exp (f : Exp.t -> Exp.t) : t -> t = function
      | Write (mb, e) -> Write (Block.map_exp f mb, f e)
      | Read (mb, v) -> Read (Block.map_exp f mb, v)

    let iter_exp (f : Exp.t -> unit) : t -> unit = function
      | Write (mb, e) ->
          Block.iter_exp f mb;
          f e
      | Read (mb, _) -> Block.iter_exp f mb

    let pp : t -> Pp.document =
      let open Pp in
      function
      | Read (mb, var) ->
          dprintf "Reading %d bits in |" (mb.size |> Size.to_bits)
          ^^ Var.pp var ^^ !^"| from: " ^^ Block.pp_addr mb
      | Write (mb, exp) ->
          dprintf "Writing %d bits with " (mb.size |> Size.to_bits)
          ^^ Exp.pp exp ^^ !^" at " ^^ Block.pp_addr mb
  end

  module SElem = struct
    type t = { base : Exp.t; end_bound : int; offsets : CCache.t }

    let len se = se.end_bound

    let make ?(end_bound = Int.max_int) base =
      debug "New selem with base %t" Pp.(top Exp.pp_smt base);
      assert (not @@ Sums.has_concrete_term base);
      { base; end_bound; offsets = CCache.empty }

    let expand_bound se nb =
      assert (nb >= se.end_bound);
      { se with end_bound = nb }

    (* Write an expression in [pos:len) in the symbolic element *)
    let write se ~pos ~len exp =
      let offsets = CCache.blit_exp exp ~pos ~len se.offsets in
      { se with offsets }

    let map_exp f se =
      let base = f se.base in
      let offsets = CCache.map_exp f se.offsets in
      { se with base; offsets }

    let iter_exp f se =
      f se.base;
      CCache.iter_exp f se.offsets

    let pp se =
      let open Pp in
      if se.end_bound = Int.max_int then
        prefix 2 1 (Exp.pp se.base ^^ space ^^ plus) (CCache.pp se.offsets)
      else
        surround 2 1
          (Exp.pp se.base ^^ space ^^ plus)
          (CCache.pp se.offsets)
          (dprintf " up to %d bytes" se.end_bound)
  end

  module SCache = struct
    include RngMap.Make (SElem)

    let map_exp f t = map (SElem.map_exp f) t

    let iter_exp f t = iter (SElem.iter_exp f) t

    let pp sc = sc |> bindings |> List.map (Pair.map Pp.shex SElem.pp) |> Pp.mapping ""
  end

  (*
     Trace and base define a much longer linked list starting from a unspecified purely symbolic
     memory state.

     ccache and scache are not part of the semantic value of the memory.

     ccache and scache domains may never overlap and this need to stay enforced.

     The semantic meaning of scache is that a symbolic block (SElem) is bounded and
     all elements inside are within the bounds even if their exact position is symbolic.
     The elements inside the selem are indexed relatively to its base by concrete offsets.

     That mean that if a symbolic write has any chance of overlapping a SElem,
     the whole SElem must be deleted.
     *)
  type t = { base : t option; trace : Event.t list; ccache : CCache.t; scache : SCache.t }

  let get_trace { base = _; trace; ccache = _; scache = _ } = trace

  let empty = { base = None; trace = []; ccache = CCache.empty; scache = SCache.empty }

  let from base = { base = Some base; trace = []; ccache = base.ccache; scache = base.scache }

  let try_read frag (block : Block.t) : Exp.t option =
    let len = Size.to_bytes block.size in
    match block.base with
    | Some bbase ->
        (* Find a matching SElem with same symbolic value given the range block.bounds *)
        let (start, endp) = Option.unlift_pair block.bounds in
        let seq = SCache.to_seq ?start ?endp frag.scache in
        let check_one (_, (selem : SElem.t)) =
          if Exp.equal selem.base bbase then CCache.sub ~pos:block.offset ~len selem.offsets
          else None
        in
        Seq.find_map check_one seq
    | None ->
        (* Concrete access: Look in ccache *)
        CCache.sub ~pos:block.offset ~len frag.ccache

  let try_read_naive (_ : t) (_ : Block.t) = Raise.todo ()

  let read_sym frag block var = { frag with trace = Read (block, var) :: frag.trace }

  let write frag (block : Block.t) exp =
    info "writing at %t" Pp.(top Block.pp block);
    let trace = Event.Write (block, exp) :: frag.trace in
    let len = Size.to_bytes block.size in
    match block.base with
    | Some bbase ->
        let (start, endp) = Option.unlift_pair block.bounds in

        (* Clearing the concrete cache on the specified area *)
        let ccache = CCache.clear_bounds ?start ?endp frag.ccache in

        (* If any block within the bounds has the same bound, we expand it instead of deleting it *)
        let seq = SCache.to_seq ?start ?endp frag.scache in
        let take_one (addr, (selem : SElem.t)) =
          if Exp.equal selem.base bbase then Some (addr, selem) else None
        in
        (* This block will be expanded (This is an option, [None] means no block will be expanded) *)
        let to_expand = Seq.find_map take_one seq in
        let new_block =
          match to_expand with
          | Some (pos, selem) -> (
              match block.bounds with
              | None ->
                  let selem = SElem.expand_bound selem Int.max_int in
                  let selem = SElem.write selem ~pos:block.offset ~len exp in
                  (selem, Int.min_int / 2)
              | Some (bstart, _) ->
                  let npos = min bstart pos in
                  let nend = max pos (SElem.len selem) in
                  let nlen = nend - npos in
                  let selem = SElem.expand_bound selem nlen in
                  let selem = SElem.write selem ~pos:block.offset ~len exp in
                  (selem, npos)
            )
          | None ->
              let (start, endp) =
                Option.value block.bounds ~default:(Int.min_int / 2, (Int.max_int - 1) / 2)
              in
              let end_bound = endp - start in
              let selem = SElem.make ~end_bound bbase in
              let selem = SElem.write selem ~pos:block.offset ~len exp in
              (selem, start)
        in
        (* Clearing the symbolic cache on that area *)
        let scache = SCache.clear_bounds ?start ?endp frag.scache in
        let scache = SCache.addp scache new_block in
        { frag with trace; ccache; scache }
    | None ->
        let ccache = CCache.blit_exp exp ~pos:block.offset ~len frag.ccache in
        let scache = SCache.clear frag.scache ~pos:block.offset ~len in
        { frag with trace; ccache; scache }

  let check_cache (_ : t) = Raise.todo ()

  let is_empty frag = frag.trace = []

  let map_exp f frag =
    let trace = List.map (Event.map_exp f) frag.trace in
    let ccache = CCache.map_exp f frag.ccache in
    let scache = SCache.map_exp f frag.scache in
    { frag with trace; ccache; scache }

  let iter_exp f frag =
    List.iter (Event.iter_exp f) frag.trace;
    CCache.iter_exp f frag.ccache;
    SCache.iter_exp f frag.scache

  let pp_raw frag =
    let open Pp in
    record "symfrag"
      [
        ("trace", list Event.pp (List.rev frag.trace));
        ("ccache", CCache.pp frag.ccache);
        ("scache", SCache.pp frag.scache);
      ]
end
