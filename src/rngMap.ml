(** A module giving a map indexing data with address ranges and
    providing access to quick access to data corresponding to any value in-between

    The ranges are not allowed to overlap.contents

    For now, this has a pure immutable interface.
 **)

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

(** The signature of the PC indexed map*)
module type S = sig
  type obj

  type obj_off = obj * int

  type t

  (** An empty rngMap *)
  val empty : t

  (** Test if an address is inside the object at address [objaddr] *)
  val is_in : objaddr:int -> obj -> int -> bool

  (** Get the object containing the address. Throw [Not_found] if no object contains the address *)
  val at : t -> int -> obj

  (** Get the object containing the address. [None] if no object contains the address *)
  val at_opt : t -> int -> obj option

  (** Get the object containing the address and the offset of the address inside the object

      Throw [Not_found] if no object contains the address *)
  val at_off : t -> int -> obj_off

  (** Get the object containing the address and the offset of the address inside the object

      [None] if no object contains the address *)
  val at_off_opt : t -> int -> obj_off option


  (** Update the binding containing the provided addr.
      If no binding contained the address, this is a no-op *)
  val update : (obj -> obj) -> t -> int -> t

  (** Clear an area of the RngMap *)
  val clear : t -> start:int -> len:int -> t

  (** Add an object at a specific address. The whole range of addresses covered by the object
      must be free *)
  val add : t -> int -> obj -> t

  (** Add an object at a specific address. The whole range of addresses covered by the object
      must be free *)
  val addp : t -> int * obj -> t

  (** Give the list of bindings *)
  val bindings : t -> (int * obj) list
end

(** Index object by range and give the object with starting *)
module Make (Obj : LenObject) : S with type obj = Obj.t = struct
  type obj = Obj.t

  type obj_off = obj * int

  type t = obj IMap.t

  let empty = IMap.empty

  let is_in ~objaddr obj addr =
    let len = Obj.len obj in
    objaddr <= addr && addr < objaddr + len

  let next t addr = IMap.find_first_opt (fun a -> a > addr) t

  let next_beg t addr = match next t addr with Some (a, obj) -> a | None -> Int.max_int

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
    match prev t addr with
    | None -> t
    | Some (objaddr, obj) -> IMap.update objaddr (Option.map f) t

  let clear t ~start ~len =
    let endp = start + len in
    let rec clear_end t start endp : t =
    match next t (start - 1) with
      |  Some (objaddr, obj) -> clear_end (IMap.remove objaddr t) objaddr endp
      | None -> t
    in
    clear_end t start endp


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

  let addp t (addr, obj) = add t addr obj

  let bindings = IMap.bindings
end
