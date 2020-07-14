(** This module introduce a type to represent the state of the machine.

    Look at {!t} for more details
 *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Support types } *)

(** This type is the standard annotation inside the state stucture.
    For now it is Isla.lrng, but it may change depending on our needs *)
type annot = Isla.lrng

let dummy_annot = Isla.UnknownRng

(** The type of registers, so basically the machine type of data we represent *)
type ty = Reg.ty

(** The type of a state ID. for now it's an integer, but it may change later *)
module Id = struct
  type t = int

  let to_string = string_of_int

  let of_string = int_of_string

  let equal : t -> t -> bool = ( = )

  let pp id = id |> to_string |> PP.string
end

type id = Id.t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State variable management } *)

(** This module provide state variables *)
module Var = struct
  (** The type of a variable in the state *)
  type t =
    | Register of Id.t * Reg.t
    | ReadVar of Id.t * int * Ast.Size.t
    | Arg of int
    | RetArg
    | RetAddr

  (** Convert the variable to the string encoding. For parsing infrastructure reason,
      the encoding must always contain at least one [:]. *)
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

  (** Expect a register variable and return the corresponding register. Throw otherwise. *)
  let expect_register = function
    | Register (_, reg) -> reg
    | v -> Raise.inv_arg "Expected register variable but got %s" (to_string v)

  (** Expect a read variable and return the corresponding index. Throw otherwise. *)
  let expect_readvar = function
    | ReadVar (_, rv, _) -> rv
    | v -> Raise.inv_arg "Expected read variable but got %s" (to_string v)

  (** The opposite of {!to_string}. Will raise [Invalid_argument] when the string don't match *)
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

  (** Create a register variable bound to the provided state id and register *)
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

  (** Hashing function for variable. For now it's polymorphic but this may stop at any time *)
  let hash = Hashtbl.hash

  (** Basically [to_string] in pp mode *)
  let pp sv = sv |> to_string |> PP.string

  (** Pretty prints but with bars around *)
  let pp_bar sv = PP.(bar ^^ pp sv ^^ bar)

  let ty = function
    | Register (_, r) -> Reg.reg_type r
    | ReadVar (_, _, size) -> Ast.Ty_BitVec (Ast.Size.to_bits size)
    | Arg _ -> Ast.Ty_BitVec 64
    | RetArg -> Ast.Ty_BitVec 64
    | RetAddr -> Ast.Ty_BitVec 64
end

type var = Var.t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State expression and typed-value management } *)

(** Module for state expressions *)
module Exp = struct
  include Exp.Make (Var)

  let of_reg id reg = Var.of_reg id reg |> of_var
end

type exp = Exp.t

type smt = (Ast.lrng, Var.t, Ast.no, Ast.no) Ast.smt

let pp_smt (smt : smt) = Ast.pp_smt Var.pp (AstManip.smt_allow_lets @@ AstManip.smt_allow_mem smt)

(** Module for optionally typed state expressions *)
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
    Exp.equal tv.exp tv'.exp && Opt.equal Ctype.equal tv.ctyp tv'.ctyp

  let pp { exp; ctyp } =
    let open PP in
    match ctyp with None -> Exp.pp exp | Some t -> infix 2 1 colon (Exp.pp exp) (Ctype.pp t)
end

type tval = Tval.t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State memory management } *)

(** This module helps managing the memory part of the state.

    This type has an imperative interface even if the underlying {!SymbolicFragment}
    has a pure interface.*)
module Mem = struct
  module Size = Ast.Size

  (** The module of state memory fragment *)
  module Fragment = SymbolicFragment.Make (Var)

  (** The index of a symbolic fragment. See {!t} for more explanations  *)
  type provenance = Ctype.provenance

  (** The type of memory. There is a main memory and a bunch of restricted fragments.
      Each of the restricted fragment has a symbolic base that should be subtracted
      from the address before accessing the fragment itself.

      In general the stack will be the fragment 0 but this is not guaranteed.
      Some execution contexts may even not have any stacks.*)
  type t = { mutable main : Fragment.t; frags : (Exp.t * Fragment.t) Vec.t }

  let empty () = { main = Fragment.empty; frags = Vec.empty () }

  let from mem =
    { main = Fragment.from mem.main; frags = Vec.map (Pair.map Fun.id Fragment.from) mem.frags }

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
    info "Writing %d bits at %t" (Size.to_bits size) PP.(top Exp.pp addr);
    (* For now bounds are never generated. This would require a great improvement
       in array analysis in the type inference system *)
    let block = Fragment.Block.make_split frag_addr size in
    let wr frag = Fragment.write frag block exp in
    update_frag ~provenance wr mem

  (** Map a function over all the memory expressions. The semantic meaning of
      expression must not change *)
  let map_mut_exp f mem =
    mem.main <- Fragment.map_exp f mem.main;
    Vec.map_mut (Pair.map f (Fragment.map_exp f)) mem.frags

  (** Iterate a function of all the memory expressions *)
  let iter_exp f mem =
    Fragment.iter_exp f mem.main;
    Vec.iter (Pair.iter f (Fragment.iter_exp f)) mem.frags

  (** Pretty print the memory *)
  let pp mem =
    let open PP in
    record ""
      [
        ("main", Fragment.pp_raw mem.main);
        ( "frags",
          Vec.ppi
            (fun (base, frag) -> PP.infix 2 1 colon (Exp.pp base) (Fragment.pp_raw frag))
            mem.frags );
      ]

  let is_empty mem =
    Fragment.is_empty mem.main && Vec.for_all (Pair.for_all Fun.ctrue Fragment.is_empty) mem.frags
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State type } *)

(** Represent the state of the machine.
    Should only be mutated when created before locking.

    The {!lock} function allow to lock a state for mutation. This is not encoded into
    the type system (yet) but it should nevertheless be respected. Once a state is locked,
    it should not be mutated anymore.

    The state are based on a dominator tree from the control flow graph structure.
    The variable in them are allowed to depend on themselves or any ancestor in the
    dominator tree. The [base_state] contains the immediate dominator. However a
    state may not represent a full node of the control flow graph but only the
    part of that node that represent control-flow coming from specific patha.
    In that case the dominator notion is only about those paths. State may only
    refer variables in among their ancestor line. This especially important when
    merging states.

    State are represented by their id and may identified to their id, they may not be
    two different state (and I mean physical equality here) with same id. See {!id2state}
*)
type t = {
  id : Id.t;
  base_state : t option;  (** The immediate dominator state in the control flow graph *)
  mutable locked : bool;  (** Tells if the state is locked *)
  mutable regs : Tval.t Reg.Map.t;
  read_vars : Tval.t Vec.t;
  mutable asserts : exp list;  (** Only asserts since base_state *)
  mem : Mem.t;
  elf : Elf.File.t option;
      (** Optionally an ELF file, this may be used when running instructions on
          the state to provide more concrete values in certain case. It *will*
          affect the execution behavior. However the symbolic execution should
          always be more concrete with it than without it *)
  fenv : Fragment.env;
  mutable last_pc : int;
      (** The PC of the instruction that lead into this state. The state should be
          right after that instruction. This has no semantic meaning as part of the state.
          It's just for helping knowing what comes from where *)
}

let equal s s' = Id.equal s.id s'.id

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State to id management } *)

(** Global map of states to associate them with identifiers *)
let id2state : (id, t) WeakMap.t = WeakMap.create 10

(** Next unused id *)
let next_id = ref 0

let of_id (id : id) = WeakMap.get id2state id

let to_id (st : t) = st.id

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State management } *)

(** Lock the state. Once a state is locked it should not be mutated anymore *)
let lock state = state.locked <- true

(** Unlock the state. This is dangerous, do not use if you do not know how to use it *)
let unsafe_unlock state = state.locked <- false

(** Tell if the state is locked, in which case it shouldn't be mutated *)
let is_locked state = state.locked

(** Tell is state is possible.

    A state is impossible if it has a single assert that is [false].

    {!StateSimplify.ctxfull} will call the SMT solver and set the assertion to that if
    required so you should call that function before [is_possible] *)
let is_possible state = match state.asserts with [Ast.Bool (false, _)] -> false | _ -> true

(** Makes a fresh state with all variable fresh and new *)
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

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retro-action.

    If the copied state is locked, then new state is based on it,
    otherwise it is a literal copy.

    The returned state is always unlocked
*)
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
      elf = Opt.(elf ||| state.elf);
      fenv = Fragment.Env.copy state.fenv;
      last_pc = state.last_pc;
    }
  in
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

(** Copy the state with {!copy} iff it is locked.
    The returned state is always unlocked *)
let copy_if_locked ?elf state = if is_locked state then copy ?elf state else state

(** Give the previous locked state in this state trace. *)
let previous state = state.base_state

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State convenience manipulation } *)

(** Add an assertion to a state *)
let push_assert (s : t) (e : exp) =
  assert (not @@ is_locked s);
  s.asserts <- e :: s.asserts

(** Map a function on all the expressions of a state by mutating. This function,
    must preserve the semantic meaning of expression (like a simplification function)
    otherwise state invariants may be broken. *)
let map_mut_exp (f : exp -> exp) s : unit =
  assert (not @@ is_locked s);
  Reg.Map.map_mut_current (Tval.map_exp f) s.regs;
  Vec.map_mut (Tval.map_exp f) s.read_vars;
  s.asserts <- List.map f s.asserts;
  Mem.map_mut_exp f s.mem

(** Iterates a function on all the expressions of a state *)
let iter_exp (f : exp -> unit) s =
  Reg.Map.iter (Tval.iter_exp f) s.regs;
  Vec.iter (Tval.iter_exp f) s.read_vars;
  List.iter f s.asserts;
  Mem.iter_exp f s.mem

(** Iterates a function on all the variables of a state *)
let iter_var (f : var -> unit) s = iter_exp (AstManip.exp_iter_var f) s

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State memory accessors } *)

(** Create a new Extra symbolic variable by mutating the state *)
let make_read (s : t) ?ctyp (size : Mem.Size.t) : var =
  assert (not @@ is_locked s);
  let len = Vec.length s.read_vars in
  let var = Var.ReadVar (s.id, len, size) in
  Vec.add_one s.read_vars { ctyp; exp = Exp.of_var var };
  var

let set_read (s : t) (read_num : int) (exp : Exp.t) =
  assert (ExpTyped.get_type exp = ExpTyped.get_type (Vec.get s.read_vars read_num |> Tval.exp));
  Vec.update s.read_vars read_num @@ Tval.map_exp (Fun.const exp)

(** Read memory from rodata.

    TODO Move this code into SymbolicFragment *)
let read_from_rodata (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t option =
  match s.elf with
  | Some elf when ConcreteEval.is_concrete addr -> (
      let int_addr = ConcreteEval.eval addr |> Value.expect_bv |> BitVec.to_int in
      let size = size |> Ast.Size.to_bits in
      try
        let (sym, offset) = Elf.SymTbl.of_addr_with_offset elf.symbols int_addr in
        if sym.writable then None
        else
          (* Assume little endian here *)
          Some (ExpTyped.bits (BytesSeq.getbvle ~size sym.data offset))
      with Not_found ->
        warn "Reading global at 0x%x which is not in a global symbol" int_addr;
        None
    )
  | _ -> None

(** Read the block designated by [addr] and [size] from the state and return an expression read.
    This will mutate the state to bind the read result to the newly created read variable.

    The [ctyp] parameter may give a type to the read variable.

    The expression could be either:
      - An actual expression if the read could be resolved.
      - Just the symbolic read variable if the read couldn't be resolved

    This function is for case with [provenance] information is known.*)
let read ~provenance ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  assert (not @@ is_locked s);
  let var = make_read ?ctyp s size in
  let exp = Mem.read s.mem ~provenance ~var ~addr ~size in
  let exp = if provenance = Main && exp = None then read_from_rodata ~addr ~size s else exp in
  Opt.iter (set_read s (Var.expect_readvar var)) exp;
  Opt.value exp ~default:(Exp.of_var var)

(** A wrapper around {!read} for use when there is no provenance information.
    It may able to still perform the read under certain condition and otherwise will fail.*)
let read_noprov ?ctyp (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) : Exp.t =
  if ConcreteEval.is_concrete addr || Vec.length s.mem.frags = 0 then
    read ~provenance:Ctype.Main ?ctyp s ~addr ~size
  else Raise.fail "Trying to access %t in state %d: No provenance info" PP.(tos Exp.pp addr) s.id

(** Write the provided value in the block. Mutate the state.*)
let write ~provenance (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  assert (not @@ is_locked s);
  Mem.write ~provenance s.mem ~addr ~size ~exp:value

(** A wrapper around {!write} for use when there is no provenance information.
    It may able to still perform the write under certain condition and otherwise will fail.*)
let write_noprov (s : t) ~(addr : Exp.t) ~(size : Mem.Size.t) (value : Exp.t) : unit =
  if ConcreteEval.is_concrete addr || Vec.length s.mem.frags = 0 then
    write ~provenance:Ctype.Main s ~addr ~size value
  else Raise.fail "Trying to access %t in state %d: No provenance info" PP.(tos Exp.pp addr) s.id

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State register accessors } *)

(** Reset the register to a symbolic value,
    and resets the type to the provided type (or no type if not provided)*)
let reset_reg (s : t) ?(ctyp : Ctype.t option) (reg : Reg.t) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ Tval.of_reg s.id ?ctyp reg

(** Sets the content of register *)
let set_reg (s : t) (reg : Reg.t) (tval : tval) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ tval

(** Sets the type of the register, leaves the value unchanged *)
let set_reg_type (s : t) (reg : Reg.t) (ctyp : Ctype.t) : unit =
  assert (not @@ is_locked s);
  let exp = Reg.Map.get s.regs reg |> Tval.exp in
  let ntval = Tval.make ~ctyp exp in
  Reg.Map.set s.regs reg ntval

(** Get the content of the register with it's type *)
let get_reg (s : t) (reg : Reg.t) : tval = Reg.Map.get s.regs reg

(** Get the content of the register without it's type *)
let get_reg_exp s reg = get_reg s reg |> Tval.exp

(** Apply a function to a register. Leave the type intact *)
let update_reg_exp (s : t) (reg : Reg.t) (f : exp -> exp) =
  Reg.Map.get s.regs reg |> Tval.map_exp f |> Reg.Map.set s.regs reg

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pc manipulation } *)

(** Set the PC to a concrete value and keep it's type appropriate *)
let set_pc ~(pc : Reg.t) (s : t) (pcval : int) =
  let exp = ExpTyped.bits_int ~size:64 pcval in
  let ctyp = Ctype.of_frag Ctype.Global ~offset:pcval ~constexpr:true in
  set_reg s pc @@ Tval.make ~ctyp exp

(** Bump a concrete PC by a concrete bump (generally the size of a non-branching instruction *)
let bump_pc ~(pc : Reg.t) (s : t) (bump : int) =
  let pc_exp = get_reg_exp s pc in
  assert (ConcreteEval.is_concrete pc_exp);
  let old_pc = ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int in
  let new_pc = old_pc + bump in
  set_pc ~pc s new_pc

(** Try to evaluate the PC if it is concrete *)
let concretize_pc ~(pc : Reg.t) (s : t) =
  let pc_exp = get_reg_exp s pc in
  try ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int |> set_pc ~pc s
  with ConcreteEval.Symbolic -> ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

let pp s =
  let open PP in
  record "state"
    [
      ("id", Id.pp s.id);
      ("base_state", Opt.fold ~none:!^"none" ~some:(fun s -> Id.pp s.id) s.base_state);
      ("last_pc", ptr s.last_pc);
      ("regs", Reg.Map.pp Tval.pp s.regs);
      ("fenv", Fragment.Env.pp s.fenv);
      ("read_vars", Vec.ppi Tval.pp s.read_vars);
      ("memory", Mem.pp s.mem);
      ("asserts", separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ Exp.pp e) s.asserts);
    ]

(** Print only the mentioned regs and the memory and asserts since the base_state.
    Until a better solution is found, the fenv will be printed entirely all the time *)
let pp_partial ~regs s =
  let open PP in
  let open Opt in
  record "state"
  @@ List.filter_map
       (function (s, Some a) -> Some (s, a) | _ -> None)
       [
         ("id", Id.pp s.id |> some);
         ("base_state", Opt.map (fun s -> Id.pp s.id) s.base_state);
         ("last_pc", ptr s.last_pc |> some);
         ( "regs",
           List.map (fun reg -> (Reg.pp reg, Reg.Map.get s.regs reg |> Tval.pp)) regs
           |> PP.mapping "" |> some );
         ("fenv", Fragment.Env.pp s.fenv |> some);
         ("read_vars", guardn (Vec.length s.read_vars = 0) @@ Vec.ppi Tval.pp s.read_vars);
         ("memory", guardn (Mem.is_empty s.mem) @@ Mem.pp s.mem);
         ( "asserts",
           guardn (s.asserts = [])
           @@ separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ Exp.pp e) s.asserts );
       ]
