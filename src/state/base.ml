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
  let next_nondet = ref 0

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
    | NonDet of int * Ast.Size.t
        (** Variable representing non-determinism in the spec.
            Can only be bit-vectors of size {8, 16, 32, 64} for now. *)
    | Section of string
        (** Symbolic base address of ELF section. Assume 64bit for now. *)

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
    | NonDet (num, size) ->
        if size = Ast.Size.B64 then Printf.sprintf "nondet:%i" num
        else Printf.sprintf "nondet:%i:%dbits" num (Ast.Size.to_bits size)
    | Section s -> "section:"^s

  let expect_register = function
    | Register (_, reg) -> reg
    | v -> Raise.inv_arg "Expected register variable but got %s" (to_string v)

  let expect_readvar = function
    | ReadVar (_, rv, _) -> rv
    | v -> Raise.inv_arg "Expected read variable but got %s" (to_string v)

  (** FIXME add nondet case *)
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
    | ["nondet"; num] ->
        let num = int_of_string num in
        NonDet (num, Ast.Size.B64)
    | ["nondet"; num; size] ->
        let num = int_of_string num in
        let size = Scanf.sscanf size "%dbits" Ast.Size.of_bits in
        NonDet (num, size)
    | ["arg"; num] -> Arg (int_of_string num)
    | ["retarg"; ""] -> RetArg
    | ["retaddr"; ""] -> RetAddr
    | ["section"; s] -> Section s
    | _ -> Raise.inv_arg "Invalid state variable: %s" s

  let of_reg id reg = Register (id, reg)

  let equal v v' =
    match (v, v') with
    | (Register (st, reg), Register (st', reg')) -> Id.equal st st' && Reg.(reg = reg')
    | (ReadVar (st, num, size), ReadVar (st', num', size')) ->
        Id.equal st st' && num = num' && size = size'
    | (Arg num, Arg num') -> num = num'
    | (RetArg, RetArg) -> true
    | (RetAddr, RetAddr) -> true
    | (NonDet (num, size), NonDet (num', size')) -> num = num' && size = size'
    | (Section s, Section s') -> s = s'
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
    | NonDet (_, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)
    | Section _ -> Ast.Ty_BitVec 64

  let new_nondet sz =
    let v = NonDet (!next_nondet, sz) in
    next_nondet := !next_nondet + 1;
    v
end

type var = Var.t

module Z3St = Z3.Make (Var)

module Sums = Exp.Sums
module Typed = Exp.Typed
module ConcreteEval = Exp.ConcreteEval
module Value = Exp.Value

module Exp = struct
  include Exp.Make (Var)

  let of_reg id reg = Var.of_reg id reg |> of_var

  let expect_sym_address exp =
    let sym, conc = Exp.Sums.split_concrete exp in
    let section = match sym with
    | Some(Ast.Var (Var.Section s, _)) -> s
    | _ -> Raise.fail "Expected symbolic Section base"
    in
    let offset = BitVec.to_int conc in
    Elf.Address.{ section; offset }
  
  let of_section ~(size : int) (section : string) =
    Typed.extract ~last:(size-1) ~first:0
        (of_var @@ Var.Section section)


  let of_address ~(size : int) (addr : Elf.Address.t) =
    Typed.(
      of_section ~size addr.section
      +
      bits_int ~size addr.offset
    )
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

module Relocation = struct
  type t = {
    value: Exp.t;
    asserts: Exp.t list;
    target: Elf.Relocations.target;
  }

  let rec exp_of_relocation_exp: Elf.Relocations.exp -> exp = 
    let f = exp_of_relocation_exp in function
    | Section s -> Exp.of_var (Var.Section s) (* TODO size? *)
    | Const x -> Typed.bits (BitVec.of_int x ~size:64) (* TODO size? *)
    | BinOp (a, Add, b) -> Typed.(f a + f b)
    | BinOp (a, Sub, b) -> Typed.(f a - f b)
    | BinOp (a, And, b) -> Typed.manyop (AstGen.Ott.Bvmanyarith AstGen.Ott.Bvand) [f a; f b]
    | UnOp (Not, b) -> Typed.unop AstGen.Ott.Bvnot (f b)

  let of_elf (relocation: Elf.Relocations.rel) = 
    let open Elf.Relocations in
    let value = exp_of_relocation_exp relocation.value in
    let asserts = List.map (function
      | Range (min, max) ->
        let min = Typed.bits @@ BitVec.of_z ~size:64 @@ Z.of_int64 min in
        let max = Typed.bits @@ BitVec.of_z ~size:64 @@ Z.of_int64 max in
        let cond1 = Typed.(binop (Bvcomp Bvsle) min value) in
        let cond2 = Typed.(binop (Bvcomp Bvslt) value max) in
        Typed.(manyop And [cond1; cond2])
      | Alignment b ->
        let last = b-1 in
        Typed.(extract ~first:0 ~last value = bits_int ~size:b 0)
    ) relocation.checks in
    let (last, first) = relocation.mask in
    let value = Typed.extract ~first ~last value in
    { value; asserts; target = relocation.target }

  module IMap = Map.Make (Int)

  let exp_of_data (data : Elf.Symbol.data) =
    let size = 8 * (BytesSeq.length data.data) in
    (* Assume little endian here *)
    let bv = BytesSeq.getbvle ~size data.data 0 in
    let exp = Typed.bits bv in
    IMap.fold (fun offset rel (exp, asserts) ->
      let relocation = of_elf rel in
      let pos = 8 * offset in
      let width = match relocation.target with
      | AArch64 Abi_aarch64_symbolic_relocation.Data640 -> 64
      | AArch64 Abi_aarch64_symbolic_relocation.Data320 -> 32
      | _ -> Raise.fail "Unsopported relocation"
      in
      let before = if pos > 0 then
        [Typed.extract ~first:0 ~last:(pos-1) exp]
      else
        []
      in
      let after = if pos + width < size then
        [Typed.extract ~first:(pos+width) ~last:(size-1) exp]
      else
        []
      in
      let v, a =
      (
        Typed.concat (after @ relocation.value :: before),
        relocation.asserts @ asserts
      ) in
      v,a
    ) data.relocations (exp, [])
end

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
  type t = {
    mutable main : Fragment.t;
    frags : (Exp.t * Fragment.t) Vec.t;
    sections : (string, provenance) Hashtbl.t; (* mapping sections to their fragments *)
    mutable allow_main : bool; (* HACK to prvent incorrectly assuming Main provenance when using section fragments  *)
  }

  (** Get the main fragment of memory *)
  let get_main { main; _ } = main

  (** Get fragment *)
  let get_frag mem i =
    Vec.get mem.frags i

  (** Empty memory, every address is unbound *)
  let empty () = { main = Fragment.empty; frags = Vec.empty (); sections = Hashtbl.create 10; allow_main = true }

  (** Build a new memory from the old one by keeping the old one as a base *)
  let from mem =
    { 
      main = Fragment.from mem.main;
      frags = Vec.map (Pair.map Fun.id Fragment.from) mem.frags;
      sections = Hashtbl.copy mem.sections;
      allow_main = mem.allow_main;
    }

  (** Copy the memory so that it can be mutated separately *)
  let copy mem = { main = mem.main; frags = Vec.copy mem.frags; sections = Hashtbl.copy mem.sections; allow_main = mem.allow_main }

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
        ("sections", hashtbl string Ctype.pp_provenance mem.sections)
      ]

  (** Check is this memory is empty which means all addresses are undefined *)
  let is_empty mem =
    Fragment.is_empty mem.main && Vec.for_all (Pair.for_all Fun.ctrue Fragment.is_empty) mem.frags


  let create_section_frag ~addr_size mem section =
    match Hashtbl.find_opt mem.sections section with
    | Some prov -> 
      info "Fragment for section %s already exists" section;
      prov
    | None ->
      let base = Exp.of_section ~size:addr_size section in
      let prov = new_frag mem base in
      Hashtbl.replace mem.sections section prov;
      prov
  
  let get_section_provenance mem section =
    Hashtbl.find_opt mem.sections section
    |> Option.value ~default:Ctype.Main
end

type t = {
  id : Id.t;
  base_state : t option;  (** The immediate dominator state in the control flow graph *)
  mutable locked : bool;  (** Tells if the state is locked *)
  mutable regs : Tval.t Reg.Map.t;  (** The values and types of registers *)
  read_vars : Tval.t Vec.t;  (** The results of reads made since base state *)
  mutable asserts : exp list;  (** Only asserts since base_state *)
  mutable relocation_asserts : exp list;  (** Only asserts since base_state *)
  mem : Mem.t;
  elf : Elf.File.t option;
      (** Optionally an ELF file, this may be used when running instructions on
          the state to provide more concrete values in certain case (like when
          reading from [.rodata]). It will affect the execution behavior.
          However the symbolic execution should always be more concrete with
          it than without it *)
  fenv : Fragment.env;  (** The memory type environment. See {!Fragment.env} *)
  mutable last_pc : Elf.Address.t;
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
      relocation_asserts = [];
      mem = Mem.empty ();
      elf;
      fenv = Fragment.Env.make ();
      last_pc = Elf.Address.{ section = ".text"; offset = 0 }; (* TODO is this right? *)
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
      relocation_asserts = (if locked then [] else state.relocation_asserts);
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

let push_relocation_assert (s : t) (e : exp) =
  assert (not @@ is_locked s);
  s.relocation_asserts <- e :: s.relocation_asserts

let rec load_relocation_asserts (s : t) =
  s.relocation_asserts @ (s.base_state |> Option.map load_relocation_asserts |> Option.value ~default:[])

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

let eval_address (s : t) (addr: Exp.t) : Elf.Address.t option =
  let ctxt0 = function Var.Section _ -> Value.bv @@ BitVec.of_int ~size:64 0 | _ -> raise ConcreteEval.Symbolic in
  let open Option in
  let* offset_exp = try
    Some (ConcreteEval.eval ~ctxt:ctxt0 addr)
  with
    ConcreteEval.Symbolic -> None
  in
  let offset = offset_exp |> Value.expect_bv |> BitVec.to_int in
  let sections = Hashtbl.create 10 in
  Ast.Manip.exp_iter_var (function Var.Section s -> Hashtbl.add sections s () | _ -> ()) addr;

  let hyps = load_relocation_asserts s in
  let size = addr |> Typed.get_type |> Typed.expect_bv in
  sections |> Hashtbl.to_seq_keys |> Seq.find_map (fun section ->
    let address = Elf.Address.{ section; offset } in
    let expression = Exp.of_address ~size address in
    if Z3St.check_full ~hyps Typed.(expression = addr) = Some true then
      Some address
    else
      None
  )
  
let read_from_rodata (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t option =
  debug "reading from rodata at address: %t" (Pp.top Exp.pp addr);
  match s.elf with
  | None -> None
  | Some elf -> (
      Option.bind (eval_address s addr) @@ fun sym_addr ->
      let size = size |> Ast.Size.to_bytes in
      try
        let (sym, offset) = Elf.SymTable.of_addr_with_offset elf.symbols sym_addr in
        if sym.writable then None
        else (
          let data = Elf.Symbol.sub sym offset size in
          let value, asserts = Relocation.exp_of_data data in
          
          if not @@ List.is_empty asserts then
            warn "Relocaiton assserts in .rodata ignored: %t" Pp.(top (list Exp.pp) asserts);

          Some value
        )
      with Not_found ->
        let int_addr = sym_addr.offset in
        let open Option in
        let* rodata = Elf.File.SMap.find_opt sym_addr.section elf.rodata in
        if rodata.addr <= int_addr && int_addr + size <= rodata.addr + rodata.size then
          let data, relocations = rodata.data in
          let data = BytesSeq.sub data (int_addr - rodata.addr) size in
          let relocations = Elf.Relocations.sub relocations (int_addr - rodata.addr) size in
          let value, asserts = Relocation.exp_of_data {data; relocations} in
          
          if not @@ List.is_empty asserts then
            warn "Relocaiton assserts in .rodata ignored: %t" Pp.(top (list Exp.pp) asserts);

          Some value
        else (
          warn "Failed to find symbol or rodata at %t" (Pp.top Elf.Address.pp sym_addr);
          None
        )
    )

let rec read ~provenance ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  assert (not @@ is_locked s);
  if provenance = Ctype.Main && not s.mem.allow_main then
    read_noprov ?ctyp s ~addr ~size
  else
    let var = make_read ?ctyp s size in
    let exp = Mem.read s.mem ~provenance ~var ~addr ~size in
    let exp = if exp = None then read_from_rodata ~addr ~size s else exp in
    Option.iter (set_read s (Var.expect_readvar var)) exp;
    Option.value exp ~default:(Exp.of_var var)

and read_noprov ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  debug "Addr: %t" Pp.(top Exp.pp addr);
  let elf_addr = eval_address s addr in
  debug "Address: %t" Pp.(top (optional Elf.Address.pp) elf_addr);
  match elf_addr with
  | Some elf_addr ->
      let addr_size = addr |> Typed.get_type |> Typed.expect_bv in
      let addr = Exp.of_address ~size:addr_size elf_addr in
      let provenance = Mem.get_section_provenance s.mem elf_addr.section in
      if provenance = Ctype.Main && not s.mem.allow_main then
        Raise.fail "Main fragment should not be used here";
      read ~provenance ?ctyp s ~addr ~size
  | None when Vec.length s.mem.frags = 0 ->
      if not s.mem.allow_main then
        Raise.fail "Main fragment should not be used here";
      read ~provenance:Ctype.Main ?ctyp s ~addr ~size
  | None -> Raise.fail "Trying to access %t in state %d: No provenance info" Pp.(tos Exp.pp addr) s.id

let rec write ~provenance (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  assert (not @@ is_locked s);
  if provenance = Ctype.Main && not s.mem.allow_main then
    write_noprov s ~addr ~size value
  else
    Mem.write ~provenance s.mem ~addr ~size ~exp:value

and write_noprov (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  let elf_addr = eval_address s addr in
  debug "Address: %t" Pp.(top (optional Elf.Address.pp) elf_addr);
  match elf_addr with
  | Some elf_addr ->
      let addr_size = addr |> Typed.get_type |> Typed.expect_bv in
      let addr = Exp.of_address ~size:addr_size elf_addr in
      let provenance = Mem.get_section_provenance s.mem elf_addr.section in
      if provenance = Ctype.Main && not s.mem.allow_main then
        Raise.fail "Main fragment should not be used here";
      write ~provenance s ~addr ~size value
  | None when Vec.length s.mem.frags = 0 ->
      if not s.mem.allow_main then
        Raise.fail "Main fragment should not be used here";
      write ~provenance:Ctype.Main s ~addr ~size value
  | None -> Raise.fail "Trying to access %t in state %d: No provenance info" Pp.(tos Exp.pp addr) s.id

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

(* TODO *)
let set_pc ~(pc : Reg.t) (s : t) (pcval : int) =
  let exp = Typed.bits_int ~size:64 pcval in
  let ctyp = Ctype.of_frag (Ctype.Global ".text") ~offset:pcval ~constexpr:true in
  set_reg s pc @@ Tval.make ~ctyp exp

let set_pc_sym ~(pc : Reg.t) (s : t) (pcval : Elf.Address.t) =
  let exp = Typed.(var ~typ:(Ty_BitVec 64) (Var.Section pcval.section) + bits_int ~size:64 pcval.offset) in
  let ctyp = Ctype.of_frag (Ctype.Global ".text") ~offset:pcval.offset ~constexpr:true in
  set_reg s pc @@ Tval.make ~ctyp exp
  

let bump_pc ~(pc : Reg.t) (s : t) (bump : int) =
  let pc_exp = get_reg_exp s pc in
  let old_pc = Exp.expect_sym_address pc_exp in
  let new_pc = Elf.Address.(old_pc + bump) in
  set_pc_sym ~pc s new_pc

let concretize_pc ~(pc : Reg.t) (s : t) =
  pc |> get_reg_exp s |> eval_address s |> Option.iter (set_pc_sym ~pc s)

let set_last_pc state pc =
  assert (not @@ is_locked state);
  state.last_pc <- pc


let push_section_constraints ~sp ~addr_size state sections =
  let sp = sp () in
  let rec f : Elf.File.section list -> unit = function
  | [] -> ()
  | s::rest -> (
    let max_section_addr = Int.shift_left 1 addr_size - s.size in
    let s_exp = (Exp.of_var (Var.Section s.name)) in
    (* The whole section fits in memory *)
    push_assert state Typed.(comp Ast.Bvule s_exp (bits_int ~size:64 max_section_addr));
    (* The load address cannot be 0 *)
    push_assert state Typed.(not (s_exp = (bits_int ~size:64 0)));
    if s.align > 1 then (
      let (align_pow, _) = Seq.ints 0
      |> Seq.drop_while (fun x -> Int.shift_left 1 x < s.align)
      |> Seq.uncons
      |> Option.get
      in
      if s.align = Int.shift_left 1 align_pow then
        let last = align_pow - 1 in
        (* Section address is aligned *)
        push_assert state Typed.(extract ~first:0 ~last s_exp = zero ~size:align_pow)
      else
        warn "Section alignment is not a power of two: %d" s.align
    );
    (* Sections don't overlap *)
    let s_end = Typed.(s_exp + bits_int ~size:64 s.size) in (* we know this doesn't overflow thanks to the other constraints *)
    List.iter (fun (s2:Elf.File.section) ->
      let s2_exp = (Exp.of_var (Var.Section s2.name)) in
      let s2_end = Typed.(s2_exp + bits_int ~size:64 s2.size) in
      let order1 = Typed.(comp Ast.Bvule s_end s2_exp) in
      let order2 = Typed.(comp Ast.Bvule s2_end s_exp) in
      push_assert state Typed.(manyop Or [order1; order2])
    ) rest;
    (* Doesn't overlap with stack *)
    let stack_end = get_reg_exp state sp in
    let stack_start = Typed.(stack_end - bits_int ~size:64 0x1000) in
    let order1 = Typed.(comp Ast.Bvule s_end stack_start) in
    let order2 = Typed.(comp Ast.Bvule stack_end s_exp) in
    push_assert state Typed.(manyop Or [order1; order2]);

    f rest
  )
  in
  f sections

let init_sections ~sp ~addr_size state =
  let state = copy_if_locked state in
  let _ = Option.(
    let+ elf = state.elf in
    state.mem.allow_main <- false;
    push_section_constraints ~sp ~addr_size state elf.sections;
    List.iter (fun (x:Elf.File.section) -> Mem.create_section_frag ~addr_size state.mem x.name |> ignore) elf.sections;
    Elf.SymTable.iter elf.symbols @@ fun sym ->
      let len = List.find (fun x -> sym.size mod x = 0) [16;8;4;2;1] in
      if sym.typ = Elf.Symbol.OBJECT then
        let provenance = Mem.get_section_provenance state.mem sym.addr.section in
        Seq.iota_step_up ~step:len ~endi:sym.size
        |> Seq.iter (fun off ->
          let data = Elf.Symbol.sub sym off len in
          let addr = Exp.of_address ~size:addr_size Elf.Address.(sym.addr + off) in
          let size = Ast.Size.of_bytes len in
          let (exp, asserts) = Relocation.exp_of_data data in
          Mem.write ~provenance state.mem ~addr ~size ~exp;
          List.iter (push_relocation_assert state) asserts;
        )
  ) in
  lock state;
  state

let init_sections_symbolic ~sp ~addr_size state =
  let state = copy_if_locked state in
  let _ = Option.(
    let+ elf = state.elf in
    push_section_constraints ~sp ~addr_size state elf.sections;
    Elf.SymTable.iter elf.symbols @@ fun sym ->
      if sym.typ = Elf.Symbol.OBJECT then
        Hashtbl.replace state.mem.sections sym.addr.section Main
  ) in
  lock state;
  state

let pp s =
  let open Pp in
  record "state"
    [
      ("id", Id.pp s.id);
      ("base_state", Option.fold ~none:!^"none" ~some:(fun s -> Id.pp s.id) s.base_state);
      ("last_pc", Elf.Address.pp s.last_pc);
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
         ("last_pc", Elf.Address.pp s.last_pc |> some);
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
