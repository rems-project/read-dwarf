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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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

(* See the .mli file for documentation. *)
open Logs.Logger (struct
  let str = __MODULE__
end)

module Id = struct
  type t = int

  let to_string = string_of_int

  let of_string = int_of_string

  let equal : t -> t -> bool = ( = )

  let pp id = id |> to_string |> Pp.string
end

type id = Id.t

module Var = struct
  type t =
    | Register of Id.t * Reg.t  (** The value of this register in this state *)
    | ReadVar of Id.t * int * Ast.Size.t
        (** The result of a certain read in a certain state.
            The size part is not semantically important:
            Two [ReadVar] with same [id] and same number may no have different sizes *)
    | Arg of int  (** A function argument *)
    | RetArg
        (** The address to which the return value should be written.
            This is used only in certain calling conventions *)
    | RetAddr  (** The return address: The address to which a "return" instruction would jump. *)

  let to_string = function
    | Register (state, reg) ->
        Printf.sprintf "reg:%s:%s" (state |> Id.to_string) (Reg.to_string reg)
    | ReadVar (state, num, size) ->
        if size = Ast.Size.B64 then Printf.sprintf "read:%s:%i" (state |> Id.to_string) num
        else
          Printf.sprintf "read:%s:%i:%dbits" (state |> Id.to_string) num (Ast.Size.to_bits size)
    | Arg num -> Printf.sprintf "arg:%i" num
    | RetArg -> "retarg:"
    | RetAddr -> "retaddr:"

  let expect_register = function
    | Register (_, reg) -> reg
    | v -> Raise.inv_arg "Expected register variable but got %s" (to_string v)

  let expect_readvar = function
    | ReadVar (_, rv, _) -> rv
    | v -> Raise.inv_arg "Expected read variable but got %s" (to_string v)

  let of_string s : t =
    match String.split_on_char ':' s with
    | ["reg"; state; reg] ->
        let state : Id.t = state |> Id.of_string in
        let reg = Reg.of_string reg in
        Register (state, reg)
    | ["read"; state; num] ->
        let state : Id.t = state |> Id.of_string in
        let num = int_of_string num in
        ReadVar (state, num, Ast.Size.B64)
    | ["read"; state; num; size] ->
        let state : Id.t = state |> Id.of_string in
        let num = int_of_string num in
        let size = Scanf.sscanf size "%dbits" Ast.Size.of_bits in
        ReadVar (state, num, size)
    | ["arg"; num] -> Arg (int_of_string num)
    | ["retarg"; ""] -> RetArg
    | ["retaddr"; ""] -> RetAddr
    | _ -> Raise.inv_arg "Invalid state variable: %s" s

  let of_reg id reg = Register (id, reg)

  let equal v v' =
    match (v, v') with
    | (Register (st, reg), Register (st', reg')) -> Id.equal st st' && Reg.equal reg reg'
    | (ReadVar (st, num, size), ReadVar (st', num', size')) ->
        Id.equal st st' && num = num' && size = size'
    | (Arg num, Arg num') -> num = num'
    | (RetArg, RetArg) -> true
    | (RetAddr, RetAddr) -> true
    | _ -> false

  let hash = Hashtbl.hash

  let pp sv = sv |> to_string |> Pp.string

  let pp_bar sv = Pp.(bar ^^ pp sv ^^ bar)

  let ty = function
    | Register (_, r) -> Reg.reg_type r
    | ReadVar (_, _, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)
    | Arg _ -> Ast.Ty_BitVec 64
    | RetArg -> Ast.Ty_BitVec 64
    | RetAddr -> Ast.Ty_BitVec 64
end

type var = Var.t

module Sums = Exp.Sums
module Typed = Exp.Typed
module ConcreteEval = Exp.ConcreteEval
module Value = Exp.Value

module Exp = struct
  include Exp.Make (Var)

  let of_reg id reg = Var.of_reg id reg |> of_var
end

type exp = Exp.t

module Tval = struct
  type t = { ctyp : Ctype.t option; exp : Exp.t }

  let make ?ctyp exp = { exp; ctyp }

  let of_exp = make

  let of_var ?ctyp var = Exp.of_var var |> of_exp ?ctyp

  let of_reg ?ctyp id reg = Exp.of_reg id reg |> of_exp ?ctyp

  let map_exp f t = { t with exp = f t.exp }

  let iter_exp f t = f t.exp

  let exp t = t.exp

  let ctyp t = t.ctyp

  let equal (tv : t) (tv' : t) =
    Exp.equal tv.exp tv'.exp && Option.equal Ctype.equal tv.ctyp tv'.ctyp

  let pp { exp; ctyp } =
    let open Pp in
    match ctyp with None -> Exp.pp exp | Some t -> infix 2 1 colon (Exp.pp exp) (Ctype.pp t)
end

type tval = Tval.t

module Mem = struct
  module Size = Ast.Size

  (** The module of state memory fragment *)
  module Fragment = SymbolicFragment.Make (Var)

  (** The index of a symbolic fragment. See {!Mem} for more explanations  *)
  type provenance = Ctype.provenance

  (** The type of memory. There is a main memory and a bunch of restricted fragments.
      Each of the restricted fragment has a symbolic base that should be subtracted
      from the address before accessing the fragment itself.

      In general the stack will be the fragment 0 but this is not guaranteed.
      Some execution contexts may even not have any stacks.*)
  type t = { mutable main : Fragment.t; frags : (Exp.t * Fragment.t) Vec.t }

  (** Get the main fragment of memory *)
  let get_main { main; frags = _ } = main

  (** Empty memory, every address is unbound *)
  let empty () = { main = Fragment.empty; frags = Vec.empty () }

  (** Build a new memory from the old one by keeping the old one as a base *)
  let from mem =
    { main = Fragment.from mem.main; frags = Vec.map (Pair.map Fun.id Fragment.from) mem.frags }

  (** Copy the memory so that it can be mutated separately *)
  let copy mem = { main = mem.main; frags = Vec.copy mem.frags }

  (** Add a new fragment with the specified base *)
  let new_frag mem base =
    Vec.add_one mem.frags (base, Fragment.empty);
    Ctype.Restricted (Vec.length mem.frags - 1)

  (** Mutate the fragment in memory designated by [provenance] with the function *)
  let update_frag ~(provenance : provenance) f mem =
    match provenance with
    | Main -> mem.main <- f mem.main
    | Restricted i ->
        let (base, frag) = Vec.get mem.frags i in
        let nfrag = f frag in
        Vec.set mem.frags i (base, nfrag)

  (** Give the fragment and in-fragment address corresponding to
      the [provenance] and [addr] given *)
  let get_frag_addr ~(provenance : provenance) mem ~(addr : Exp.t) =
    match provenance with
    | Main -> (mem.main, addr)
    | Restricted i ->
        let (term, frag) = Vec.get mem.frags i in
        (frag, Sums.smart_substract ~equal:Exp.equal ~term addr)

  (** Read a value of size [size] in memory at address [addr] in the fragment designated
      by [provenance] into variable [var]. If the read can be resolved from previous writes,
      then an expression is returned, otherwise [None] is returned *)
  let read ~provenance mem ~var ~(addr : Exp.t) ~size : exp option =
    let (frag, frag_addr) = get_frag_addr ~provenance mem ~addr in
    (* For now bounds are never generated. This would require a great improvement
       in array analysis in the type inference system *)
    let block = Fragment.Block.make_split frag_addr size in
    update_frag ~provenance (fun frag -> Fragment.read_sym frag block var) mem;
    Fragment.try_read frag block

  (** Write a value [exp] of size [size] in memory at address [addr] in the fragment
      designated by [provenance]. *)
  let write ~provenance mem ~addr ~size ~exp : unit =
    let (_, frag_addr) = get_frag_addr ~provenance mem ~addr in
    info "Writing %d bits at %t" (Size.to_bits size) Pp.(top Exp.pp addr);
    (* For now bounds are never generated. This would require a great improvement
       in array analysis in the type inference system *)
    let block = Fragment.Block.make_split frag_addr size in
    let wr frag = Fragment.write frag block exp in
    update_frag ~provenance wr mem

  (** Map a function over all the memory expressions. The semantic meaning of
      expressions must not change *)
  let map_mut_exp f mem =
    mem.main <- Fragment.map_exp f mem.main;
    Vec.map_mut (Pair.map f (Fragment.map_exp f)) mem.frags

  (** Iterate a function of all the memory expressions *)
  let iter_exp f mem =
    Fragment.iter_exp f mem.main;
    Vec.iter (Pair.iter f (Fragment.iter_exp f)) mem.frags

  (** Pretty print the memory *)
  let pp mem =
    let open Pp in
    record ""
      [
        ("main", Fragment.pp_raw mem.main);
        ( "frags",
          Vec.ppi
            (fun (base, frag) -> Pp.infix 2 1 colon (Exp.pp base) (Fragment.pp_raw frag))
            mem.frags );
      ]

  (** Check is this memory is empty which means all addresses are undefined *)
  let is_empty mem =
    Fragment.is_empty mem.main && Vec.for_all (Pair.for_all Fun.ctrue Fragment.is_empty) mem.frags
end

type t = {
  id : Id.t;
  base_state : t option;  (** The immediate dominator state in the control flow graph *)
  mutable locked : bool;  (** Tells if the state is locked *)
  mutable regs : Tval.t Reg.Map.t;  (** The values and types of registers *)
  read_vars : Tval.t Vec.t;  (** The results of reads made since base state *)
  mutable asserts : exp list;  (** Only asserts since base_state *)
  mem : Mem.t;
  elf : Elf.File.t option;
      (** Optionally an ELF file, this may be used when running instructions on
          the state to provide more concrete values in certain case (like when
          reading from [.rodata]). It will affect the execution behavior.
          However the symbolic execution should always be more concrete with
          it than without it *)
  fenv : Fragment.env;  (** The memory type environment. See {!Fragment.env} *)
  mutable last_pc : int;
      (** The PC of the instruction that lead into this state. The state should be
          right after that instruction. This has no semantic meaning as part of the state.
          It's just for helping knowing what comes from where *)
}

let equal s s' = Id.equal s.id s'.id

let id2state : (id, t) WeakMap.t = WeakMap.create 10

let next_id = ref 0

let of_id (id : id) = WeakMap.get id2state id

let to_id (st : t) = st.id

let lock state = state.locked <- true

let unsafe_unlock state = state.locked <- false [@@deprecated "Stop unlocking states"]

let is_locked state = state.locked

let is_possible state = match state.asserts with [Ast.Bool (false, _)] -> false | _ -> true

let make ?elf () =
  let id = !next_id in
  let state =
    {
      id;
      base_state = None;
      locked = false;
      regs = Reg.Map.init @@ Tval.of_reg id;
      read_vars = Vec.empty ();
      asserts = [];
      mem = Mem.empty ();
      elf;
      fenv = Fragment.Env.make ();
      last_pc = 0;
    }
  in
  next_id := id + 1;
  WeakMap.add id2state id state;
  state

let copy ?elf state =
  let id = !next_id in
  let locked = is_locked state in
  let nstate =
    {
      id;
      base_state = (if locked then Some state else state.base_state);
      locked = false;
      regs = Reg.Map.copy state.regs;
      read_vars = Vec.empty ();
      asserts = (if locked then [] else state.asserts);
      mem = (if locked then Mem.from state.mem else Mem.copy state.mem);
      elf = Option.(elf ||| state.elf);
      fenv = Fragment.Env.copy state.fenv;
      last_pc = state.last_pc;
    }
  in
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

let copy_if_locked ?elf state = if is_locked state then copy ?elf state else state

let push_assert (s : t) (e : exp) =
  assert (not @@ is_locked s);
  s.asserts <- e :: s.asserts

let set_asserts state asserts =
  assert (not @@ is_locked state);
  state.asserts <- asserts

let set_impossible state =
  assert (not @@ is_locked state);
  state.asserts <- [Typed.false_]

let map_mut_exp (f : exp -> exp) s : unit =
  assert (not @@ is_locked s);
  Reg.Map.map_mut_current (Tval.map_exp f) s.regs;
  Vec.map_mut (Tval.map_exp f) s.read_vars;
  s.asserts <- List.map f s.asserts;
  Mem.map_mut_exp f s.mem

let iter_exp (f : exp -> unit) s =
  Reg.Map.iter (Tval.iter_exp f) s.regs;
  Vec.iter (Tval.iter_exp f) s.read_vars;
  List.iter f s.asserts;
  Mem.iter_exp f s.mem

let iter_var (f : var -> unit) s = iter_exp (Ast.Manip.exp_iter_var f) s

let make_read (s : t) ?ctyp (size : Mem.Size.t) : var =
  assert (not @@ is_locked s);
  let len = Vec.length s.read_vars in
  let var = Var.ReadVar (s.id, len, size) in
  Vec.add_one s.read_vars { ctyp; exp = Exp.of_var var };
  var

let set_read (s : t) (read_num : int) (exp : Exp.t) =
  assert (Typed.get_type exp = Typed.get_type (Vec.get s.read_vars read_num |> Tval.exp));
  Vec.update s.read_vars read_num @@ Tval.map_exp (Fun.const exp)

let read_from_rodata (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t option =
  match s.elf with
  | Some elf when ConcreteEval.is_concrete addr -> (
      let int_addr = ConcreteEval.eval addr |> Value.expect_bv |> BitVec.to_int in
      let size = size |> Ast.Size.to_bits in
      try
        let (sym, offset) = Elf.SymTable.of_addr_with_offset elf.symbols int_addr in
        if sym.writable then None
        else
          (* Assume little endian here *)
          Some (Typed.bits (BytesSeq.getbvle ~size sym.data offset))
      with Not_found ->
        warn "Reading global at 0x%x which is not in a global symbol" int_addr;
        None
    )
  | _ -> None

let read ~provenance ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  assert (not @@ is_locked s);
  let var = make_read ?ctyp s size in
  let exp = Mem.read s.mem ~provenance ~var ~addr ~size in
  let exp = if provenance = Main && exp = None then read_from_rodata ~addr ~size s else exp in
  Option.iter (set_read s (Var.expect_readvar var)) exp;
  Option.value exp ~default:(Exp.of_var var)

let read_noprov ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  if ConcreteEval.is_concrete addr || Vec.length s.mem.frags = 0 then
    read ~provenance:Ctype.Main ?ctyp s ~addr ~size
  else Raise.fail "Trying to access %t in state %d: No provenance info" Pp.(tos Exp.pp addr) s.id

let write ~provenance (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  assert (not @@ is_locked s);
  Mem.write ~provenance s.mem ~addr ~size ~exp:value

let write_noprov (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  if ConcreteEval.is_concrete addr || Vec.length s.mem.frags = 0 then
    write ~provenance:Ctype.Main s ~addr ~size value
  else Raise.fail "Trying to access %t in state %d: No provenance info" Pp.(tos Exp.pp addr) s.id

let reset_reg (s : t) ?(ctyp : Ctype.t option) (reg : Reg.t) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ Tval.of_reg s.id ?ctyp reg

let set_reg (s : t) (reg : Reg.t) (tval : tval) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ tval

let set_reg_type (s : t) (reg : Reg.t) (ctyp : Ctype.t) : unit =
  assert (not @@ is_locked s);
  let exp = Reg.Map.get s.regs reg |> Tval.exp in
  let ntval = Tval.make ~ctyp exp in
  Reg.Map.set s.regs reg ntval

let get_reg (s : t) (reg : Reg.t) : tval = Reg.Map.get s.regs reg

let get_reg_exp s reg = get_reg s reg |> Tval.exp

let update_reg_exp (s : t) (reg : Reg.t) (f : exp -> exp) =
  Reg.Map.get s.regs reg |> Tval.map_exp f |> Reg.Map.set s.regs reg

let set_pc ~(pc : Reg.t) (s : t) (pcval : int) =
  let exp = Typed.bits_int ~size:64 pcval in
  let ctyp = Ctype.of_frag Ctype.Global ~offset:pcval ~constexpr:true in
  set_reg s pc @@ Tval.make ~ctyp exp

let bump_pc ~(pc : Reg.t) (s : t) (bump : int) =
  let pc_exp = get_reg_exp s pc in
  assert (ConcreteEval.is_concrete pc_exp);
  let old_pc = ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int in
  let new_pc = old_pc + bump in
  set_pc ~pc s new_pc

let concretize_pc ~(pc : Reg.t) (s : t) =
  let pc_exp = get_reg_exp s pc in
  try ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int |> set_pc ~pc s
  with ConcreteEval.Symbolic -> ()

let set_last_pc state pc =
  assert (not @@ is_locked state);
  state.last_pc <- pc

let pp s =
  let open Pp in
  record "state"
    [
      ("id", Id.pp s.id);
      ("base_state", Option.fold ~none:!^"none" ~some:(fun s -> Id.pp s.id) s.base_state);
      ("last_pc", ptr s.last_pc);
      ("regs", Reg.Map.pp Tval.pp s.regs);
      ("fenv", Fragment.Env.pp s.fenv);
      ("read_vars", Vec.ppi Tval.pp s.read_vars);
      ("memory", Mem.pp s.mem);
      ("asserts", separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ Exp.pp e) s.asserts);
    ]

let pp_partial ~regs s =
  let open Pp in
  let open Option in
  record "state"
  @@ List.filter_map
       (function (s, Some a) -> Some (s, a) | _ -> None)
       [
         ("id", Id.pp s.id |> some);
         ("base_state", Option.map (fun s -> Id.pp s.id) s.base_state);
         ("last_pc", ptr s.last_pc |> some);
         ( "regs",
           List.map (fun reg -> (Reg.pp reg, Reg.Map.get s.regs reg |> Tval.pp)) regs
           |> Pp.mapping "" |> some );
         ("fenv", Fragment.Env.pp s.fenv |> some);
         ("read_vars", guardn (Vec.length s.read_vars = 0) @@ Vec.ppi Tval.pp s.read_vars);
         ("memory", guardn (Mem.is_empty s.mem) @@ Mem.pp s.mem);
         ( "asserts",
           guardn (s.asserts = [])
           @@ separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ Exp.pp e) s.asserts );
       ]
