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

(** A module giving a map indexing data with address ranges and providing access
    to quick access to data corresponding to any value in-between.

    Each address is bound to at most one object so the ranges are not allowed to
    overlap.

    In practice you dont specify the range. The size of the bound range is
    provided by the stored object itself that is assumed to have a length. The
    stored object must thus provide a [len] function as specfified by the
    {!LenObject} signature. However a length can be added to any {!Object} using
    {!PairLenObject}

    An important remark is that maps are perfectly allowed to have negative
    addresses, and will considered negative integer as negative addresses. An
    object of size 7 starting at -4 will end at 3

    For now, this has a pure immutable interface.*)

(** An integer map: [Map.Make(Int)] *)
module IMap = Map.Make (Int)

(** A module that represent a simple type with no operation *)
module type Object = sig
  (** A type, for functors that accept generic types *)
  type t
end

(** A module for type that have a concept of length in the rngMap context *)
module type LenObject = sig
  (** The type to be indexed by starting addresses, must have a length *)
  type t

  (** The type of range end *)
  val len : t -> int
end

(** For types that do not have an inner length representation, we add it with a pair *)
module PairLenObject (Obj : Object) : LenObject with type t = Obj.t * int = struct
  type t = Obj.t * int

  let len (_, l) = l
end

(** The signature of the range map*)
module type S = sig
  (** The type of the contained object *)
  type obj

  (** The type of an object with an offset *)
  type obj_off = obj * int

  (** The type of the map from address ranges to {!obj} *)
  type t

  (** An empty [RngMap] *)
  val empty : t

  (** Test if an address is inside the object at address [objaddr] *)
  val is_in : objaddr:int -> obj -> int -> bool

  (** Get the object containing the address. Throw [Not_found] if no object contains the address *)
  val at : t -> int -> obj

  (** Get the object containing the address. [None] if no object contains the address *)
  val at_opt : t -> int -> obj option

  (** Get the object containing the address and the offset of the address inside the object

      {[at_off map addr = (obj, off) ]}

      means:

      {v
         |                      |           |         |
       map 0                 obj start    point    obj end
         |<--------------addr-------------->|
                                |<---off--->|
                                |<------len obj------>|
      v}

      In other words, [at_off] allow a change of coordinate from the map frame to the
      object frame.

      Throw [Not_found] if no object contains the address *)
  val at_off : t -> int -> obj_off

  (** Get the object containing the address and the offset of the address inside the object.
      See {!at_off} for more explanation.

      [None] if no object contains the address *)
  val at_off_opt : t -> int -> obj_off option

  (** Update the binding containing the provided address.
      If no binding contained the address, this is a no-op *)
  val update : (obj -> obj) -> t -> int -> t

  (** Map a function over all the objects *)
  val map : (obj -> obj) -> t -> t

  (** Map a function over all the objects with their address *)
  val mapi : (int -> obj -> obj) -> t -> t

  (** Iter a function over all the objects *)
  val iter : (obj -> unit) -> t -> unit

  (** Iter a function over all the objects with their address *)
  val iteri : (int -> obj -> unit) -> t -> unit

  (** Clear the object containing the address if any *)
  val clear_at : t -> int -> t

  (** Clear an area of the RngMap.

      If an object is partially in the specified block. It will be removed entirely.

      See {!clear_crop} for a different behavior.
      See {!clear_bounds} to allow some bounds to be infinity.
  *)
  val clear : t -> pos:int -> len:int -> t

  (** Clear an area of the RngMap.

      If a block is partially in the specified block, It will be cropped by
      using the provided crop function.

      [crop ~pos ~len obj] is supposed to crop the object [obj] and keep only the segment
      [\[pos:pos +len)] of it (in the object coordinate frame).*)
  val clear_crop : t -> pos:int -> len:int -> crop:(pos:int -> len:int -> obj -> obj) -> t

  (** Same as {!clear} but if a bound is missing, then we erase until infinity in
      that direction. The target interval is [\[start:endp)].

      In particular [clear_bounds map = ]{!empty}.*)
  val clear_bounds : ?start:int -> ?endp:int -> t -> t

  (** Add an object at a specific address. The whole range of addresses covered by the object
      must be free *)
  val add : t -> int -> obj -> t

  (** Add an object at a specific address. The whole range of addresses covered by the object
      must be free *)
  val addp : t -> obj_off -> t

  (** Give the list of bindings *)
  val bindings : t -> (int * obj) list

  (** Return a sequence of all the object overlapping the range [\[start:endp)].
      The first and last element may not be entierly contained in the ranged.
      If any bound is unspecified, it goes to infinity in that direction.


      In particular [to_seq map] will iterate the entiere [RngMap]*)
  val to_seq : ?start:int -> ?endp:int -> t -> (int * obj) Seq.t
end

(*****************************************************************************)
(*        Implementation                                                     *)
(*****************************************************************************)

(** How to make a [RngMap] from a {!LenObject} *)
module Make (Obj : LenObject) : S with type obj = Obj.t = struct
  type obj = Obj.t

  type obj_off = obj * int

  type t = obj IMap.t

  let empty = IMap.empty

  let is_in ~objaddr obj addr =
    let len = Obj.len obj in
    objaddr <= addr && addr < objaddr + len

  let next t addr = IMap.find_first_opt (fun a -> a > addr) t

  let next_beg t addr = match next t addr with Some (a, _) -> a | None -> Int.max_int

  let prev t addr = IMap.find_last_opt (fun a -> a <= addr) t

  let prev_end t addr =
    match prev t addr with Some (a, obj) -> a + Obj.len obj | None -> Int.min_int

  let at_opt t addr =
    Option.bind (prev t addr) (fun (objaddr, candidate) ->
        if is_in ~objaddr candidate addr then Some candidate else None)

  let at t addr = match at_opt t addr with Some o -> o | None -> raise Not_found

  let at_off_opt t addr =
    match prev t addr with
    | Some (objaddr, candidate) ->
        if is_in ~objaddr candidate addr then Some (candidate, addr - objaddr) else None
    | None -> None

  let at_off t addr =
    match prev t addr with
    | Some (objaddr, candidate) ->
        if is_in ~objaddr candidate addr then (candidate, addr - objaddr) else raise Not_found
    | None -> raise Not_found

  let update f t addr =
    match prev t addr with None -> t | Some (objaddr, _) -> IMap.update objaddr (Option.map f) t

  let map = IMap.map

  let mapi = IMap.mapi

  let iter f m = IMap.iter (fun _ a -> f a) m

  let iteri = IMap.iter

  let to_seq ?start ?endp t =
    let start_seq =
      match start with
      | None -> IMap.to_seq t
      | Some s -> (
          let start_seq = IMap.to_seq_from s t in
          match prev t (s - 1) with
          | Some (addr, obj) when addr + Obj.len obj > s -> Seq.cons (addr, obj) start_seq
          | _ -> start_seq
        )
    in
    match endp with
    | Some e -> start_seq |> Seq.stop_at (fun (i, _) -> i >= e)
    | None -> start_seq

  let clear_at t pos =
    match prev t pos with
    | Some (addr, obj) when addr + Obj.len obj > pos -> IMap.remove addr t
    | _ -> t

  let clear t ~pos ~len =
    assert (len >= 0);
    (* Remove an possible object starting before [pos] but ending after [pos]. *)
    let t = clear_at t pos in
    let seq = IMap.to_seq_from pos t in
    let endp = pos + len in
    let rec remove_until t seq endp =
      match seq () with
      | Seq.Cons ((addr, _), seq) when addr < endp -> remove_until (IMap.remove addr t) seq endp
      | _ -> t
    in
    remove_until t seq endp

  let clear_bounds ?start ?endp t =
    assert (endp >= start);
    match (start, endp) with
    | (None, None) -> empty
    | (Some s, None) ->
        let t = clear_at t s in
        let (res, m, _) = IMap.split s t in
        assert (m = None);
        res
    | (None, Some e) ->
        let (_, _, res) = IMap.split (e - 1) t in
        res
    | (Some s, Some e) -> clear t ~pos:s ~len:(e - s)

  let clear_crop t ~pos ~len ~crop =
    assert (len >= 0);
    (* Crop an possible object starting before the start but ending after the start. *)
    let endp = pos + len in
    let t =
      match prev t (pos - 1) with
      | Some (addr, obj) when addr + Obj.len obj > pos ->
        let objend = addr + Obj.len obj in 
        let t = if endp < objend then
            IMap.add (pos + len) (crop ~pos:(endp - addr) ~len:(objend - endp) obj) t
          else
            t
          in
          IMap.update addr (Option.map (crop ~pos:0 ~len:(pos - addr))) t
      | _ -> t
    in
    let seq = IMap.to_seq_from pos t in
    (* Remove all objects of the sequence from t until endp *)
    let rec remove_until t seq endp =
      match seq () with
      | Seq.Cons ((addr, obj), seq) when addr < endp ->
          let endobj = addr + Obj.len obj in
          if endobj > endp then
            (* The end of the object id beyond the end of the clearing window.
                                   This is the last object to clear and it need to be cropped *)
            let nobj = crop ~pos:(endp - addr) ~len:(endobj - endp) obj in
            let t = IMap.remove addr t in
            let t = IMap.add endp nobj t in
            t
          else remove_until (IMap.remove addr t) seq endp
      | _ -> t
    in
    remove_until t seq endp

  let unsafe_add t addr obj = IMap.add addr obj t

  let add t addr obj =
    let pend = prev_end t addr in
    let nbegin = next_beg t addr in
    if pend <= addr && nbegin >= addr + Obj.len obj then unsafe_add t addr obj
    else
      Raise.inv_arg
        "RngMap.add : Space occupied when inserting in [0x%x;0x%x): previous block end at 0x%x \
         and next block begins at 0x%x"
        addr
        (addr + Obj.len obj)
        pend nbegin

  let addp t (obj, addr) = add t addr obj

  let bindings = IMap.bindings
end
