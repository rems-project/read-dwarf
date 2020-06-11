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

type 'a vector = 'a Vec.t

(** This type is the standard annotation inside the state stucture.
    For now it is Isla.lrng, but it may change depending on our needs *)
type annot = Isla.lrng

let dummy_annot = Isla.UnknownRng

type ty = Reg.ty

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
(** {1 State types } *)

(** All the possible atomic access sizes *)
type mem_size = Ast.Size.t

(** Represent the state of the machine.
    Should only be mutated when created before locking.

    The {!lock} function allow to lock a state for mutation. This is not encoded into
    the type system (yet) but it should nevertheless be respected. Once a state is locked,
    it should not be mutated anymore.
*)
type t = {
  id : id;
  base_state : t option;
  (* The immediate dominator state in the control flow graph *)
  mutable regs : tval Reg.Map.t;
  read_vars : (ty * tval) vector;
  mutable asserts : exp list;
  (* Only asserts since base_state *)
  mem : mem;
  elf : Elf.File.t option;
  fenv : Fragment.env;
  mutable last_pc : int;
      (** The PC of the instruction that lead into this state. The state should be
          right after that instruction. This has no semantic meaning as part of the state.
          It's just for helping knowing what comes from where *)
}

and tval = { ctyp : Ctype.t option; exp : exp }

and exp = (Ast.lrng, var, Ast.no, Ast.no) Ast.exp

and var = Register of t * Reg.t | ReadVar of t * int | Arg of int | RetArg | RetAddr

and mem_block = { addr : exp; size : mem_size }

and mem_event =
  | Read of mem_block * var
  | Write of mem_block * exp
  | Lock of t
      (** Represents that all the memory operation below were already in the given state *)

and mem = { mutable trace : mem_event list }

(* Other ast aliases *)

type smt = (Ast.lrng, var, Ast.no, Ast.no) Ast.smt

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
(** {1 State variable management } *)

module Var = struct
  type state = t

  type t = var

  (** Convert the variable to the string encoding. For parsing infrastructure reason,
      the encoding must always contain at least one [:]. *)
  let to_string = function
    | Register (state, reg) ->
        Printf.sprintf "reg:%s:%s" (state |> to_id |> Id.to_string) (Reg.to_string reg)
    | ReadVar (state, num) -> Printf.sprintf "read:%s:%i" (state |> to_id |> Id.to_string) num
    | Arg num -> Printf.sprintf "arg:%i" num
    | RetArg -> "retarg:"
    | RetAddr -> "retaddr:"

  let of_string s : t =
    match String.split_on_char ':' s with
    | ["reg"; state; reg] ->
        let state : state = state |> Id.of_string |> of_id in
        let reg = Reg.of_string reg in
        Register (state, reg)
    | ["read"; state; num] ->
        let state : state = state |> Id.of_string |> of_id in
        let num = int_of_string num in
        ReadVar (state, num)
    | ["arg"; num] -> Arg (int_of_string num)
    | ["retarg"; ""] -> RetArg
    | ["retaddr"; ""] -> RetAddr
    | _ -> Raise.inv_arg "Invalid state variable: %s" s

  let to_exp (v : t) : exp = Ast.Var (v, dummy_annot)

  let equal v v' =
    match (v, v') with
    | (Register (st, reg), Register (st', reg')) -> equal st st' && Reg.equal reg reg'
    | (ReadVar (st, num), ReadVar (st', num')) -> equal st st' && num = num'
    | (Arg num, Arg num') -> num = num'
    | (RetArg, RetArg) -> true
    | (RetAddr, RetAddr) -> true
    | _ -> false

  let pp sv = sv |> to_string |> PP.string

  let pp_bar sv = PP.(bar ^^ pp sv ^^ bar)
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State expression and typed-value management } *)

let make_tval ?ctyp exp = { exp; ctyp }

let tval_map_exp f t = { t with exp = f t.exp }

let get_exp tval = tval.exp

let get_ctyp tval = tval.ctyp

let equal_exp (e : exp) (e' : exp) = Ast.equal_exp ~var:Var.equal e e'

let equal_tval (tv : tval) (tv' : tval) =
  equal_exp tv.exp tv'.exp && Opt.equal Ctype.equal tv.ctyp tv'.ctyp

let pp_exp_smt (exp : exp) = Ast.pp_exp Var.pp (AstManip.allow_lets @@ AstManip.allow_mem exp)

let pp_exp (exp : exp) = PPExp.pp_exp Var.pp_bar exp

let pp_smt (smt : smt) = Ast.pp_smt Var.pp (AstManip.smt_allow_lets @@ AstManip.smt_allow_mem smt)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State memory management } *)

(** This module helps managing the memory part of the state. *)
module Mem = struct
  module Size = Ast.Size

  type block = mem_block

  type event = mem_event

  type t = mem

  let equal_size = Ast.Size.equal

  let equal_block bl bl' = equal_exp bl.addr bl'.addr && equal_size bl.size bl'.size

  let equal_event e e' =
    match (e, e') with
    | (Read (mb, v), Read (mb', v')) -> Var.equal v v' && equal_block mb mb'
    | (Write (mb, e), Write (mb', e')) -> equal_exp e e' && equal_block mb mb'
    | (Lock st, Lock st') -> equal st st'
    | _ -> false

  (* TODO rewrite that to stop the search at the first lock *)
  let equal m m' = List.equal equal_event m.trace m'.trace

  let empty () = { trace = [] }

  let make state = { trace = [] }

  let read mem mb var = mem.trace <- Read (mb, var) :: mem.trace

  let write mem mb value = mem.trace <- Write (mb, value) :: mem.trace

  let lock mem state = mem.trace <- Lock state :: mem.trace

  let unlock mem =
    match mem.trace with
    | Lock _ :: l -> mem.trace <- l
    | _ -> failwith "Can't unlock the unlocked"

  let copy mem = { trace = mem.trace }

  let map_mut_exp (f : exp -> exp) (mem : t) : unit =
    let rec map_until_lock = function
      | [] -> []
      | Lock _ :: l as res -> res
      | Write ({ addr; size }, value) :: l ->
          Write ({ addr = f addr; size }, f value) :: map_until_lock l
      | Read ({ addr; size }, var) :: l -> Read ({ addr = f addr; size }, var) :: map_until_lock l
    in
    mem.trace <- map_until_lock mem.trace

  let map_exp (f : exp -> exp) (mem : t) : t =
    let res = copy mem in
    map_mut_exp f res;
    res

  let iter_exp (f : exp -> unit) (mem : t) : unit =
    List.iter
      (function
        | Read ({ addr; size }, var) -> f addr
        | Write ({ addr; size }, value) ->
            f addr;
            f value
        | Lock _ -> ())
      mem.trace

  let pp_size size = PP.(dprintf "%dbits" (Size.to_bits size))

  let pp_block block =
    PP.(pp_size block.size ^^ surround 2 0 lbracket (pp_exp block.addr) rbracket)

  let pp_event : event -> PP.document = function
    | Lock s -> PP.(dprintf "Lock %d" s.id)
    | Read (mb, var) ->
        PP.(
          dprintf "Reading %d bits in |" (Size.to_bits mb.size)
          ^^ Var.pp var ^^ !^"| from: " ^^ pp_exp mb.addr)
    | Write (mb, exp) ->
        PP.(
          dprintf "Writing %d bits with " (Size.to_bits mb.size)
          ^^ pp_exp exp ^^ !^" at " ^^ pp_exp mb.addr)

  let pp mem = PP.prefix 2 1 (PP.string "Full trace:") (PP.list pp_event (List.rev mem.trace))
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State management } *)

(** Lock the state. Once a state is locked it should not be mutated anymore *)
let lock state = Mem.lock state.mem state

(** Unlock the state. This is dangerous, do not use if you do not know how to use it *)
let unsafe_unlock state = Mem.unlock state.mem

let is_locked state =
  match state.mem.trace with Lock s :: _ when s.id = state.id -> true | _ -> false

(** Tell is state is possible.

    A state is impossible if it has a single assert that is [false].

    {!StateSimplify.ctxfull} will call the SMT solver and set the assertion to that if
    required so you should call that function before [is_possible] *)
let is_possible state = match state.asserts with [Ast.Bool (false, _)] -> false | _ -> true

let make_reg_cell state ?ctyp p = { exp = Var.to_exp (Register (state, p)); ctyp }

(** Makes a fresh state with all variable fresh and new *)
let make ?elf () =
  let id = !next_id in
  let state =
    {
      id;
      base_state = None;
      regs = Reg.Map.dummy ();
      read_vars = Vec.empty ();
      asserts = [];
      mem = Mem.empty ();
      elf;
      fenv = Fragment.Env.make ();
      last_pc = 0;
    }
  in
  (* Warning: here I'm creating a cyclic reference. Please be aware of that *)
  state.regs <- Reg.Map.init @@ make_reg_cell state;
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
      regs = Reg.Map.copy state.regs;
      read_vars = Vec.empty ();
      asserts = (if locked then [] else state.asserts);
      mem = Mem.copy state.mem;
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

(** Map a function on all the expressions of a state by mutating *)
let map_mut_exp (f : exp -> exp) s : unit =
  assert (not @@ is_locked s);
  let tval_map tv = { tv with exp = f tv.exp } in
  Reg.Map.map_mut_current tval_map s.regs;
  Vec.map_mut (Pair.map Fun.id tval_map) s.read_vars;
  s.asserts <- List.map f s.asserts;
  Mem.map_mut_exp f s.mem

(** Copy the state and map all the expression on it. The new state is unlocked *)
let map_exp (f : exp -> exp) s : t =
  let res = copy s in
  map_mut_exp f res;
  res

(** Iterates a function on all the expressions of a state *)
let iter_exp (f : exp -> unit) s =
  let tval_iter tv = f tv.exp in
  Reg.Map.iter tval_iter s.regs;
  Vec.iter (Pair.iter ignore tval_iter) s.read_vars;
  List.iter f s.asserts;
  Mem.iter_exp f s.mem

(** Iterates a function on all the variables of a state *)
let iter_var (f : var -> unit) s = iter_exp (AstManip.exp_iter_var f) s

(** Return the type of a state variable *)
let var_type var =
  match var with
  | Register (state, r) -> Reg.reg_type r
  | ReadVar (state, i) -> Vec.get state.read_vars i |> fst
  | Arg _ -> Ast.Ty_BitVec 64
  | RetArg -> Ast.Ty_BitVec 64
  | RetAddr -> Ast.Ty_BitVec 64

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State memory accessors } *)

(** Create a new Extra symbolic variable by mutating the state *)
let make_read (s : t) ?ctyp ?exp (ty : ty) : var =
  assert (not @@ is_locked s);
  let len = Vec.length s.read_vars in
  let var = ReadVar (s, len) in
  Vec.add_one s.read_vars (ty, { ctyp; exp = Opt.value exp ~default:(Var.to_exp var) });
  var

(** Read the block designated by mb from the state and return an expression read.
    This may mutate the state to add the read of the state as

    Depending on the inputs and the setting, this expression could be:
    - a symbolic Extra variable that has a read event in the trace
    - a concrete constant (read from rodata)
    - a symbolic initial memory variable (when and if those exist)

    In practice for now it is always the first case *)
let read ?ctyp (s : t) (mb : Mem.block) : exp =
  assert (not @@ is_locked s);
  let ty = Mem.Size.to_bv mb.size in
  let exp =
    match s.elf with
    | Some elf when ConcreteEval.is_concrete mb.addr -> (
        let int_addr = ConcreteEval.eval mb.addr |> Value.expect_bv |> BitVec.to_int in
        let size = mb.size |> Ast.Size.to_bits in
        try
          let (sym, offset) = Elf.SymTbl.of_addr_with_offset elf.symbols int_addr in
          if sym.writable then None
          else
            (* Assume little endian here *)
            Some (Ast.Op.bits (BytesSeq.getbvle ~size sym.data offset))
        with Not_found ->
          warn "Reading global at 0x%x which is not in a global symbol" int_addr;
          None
      )
    | _ -> None
  in
  let nvar = make_read ?ctyp ?exp s ty in
  Mem.read s.mem mb nvar;
  Opt.value exp ~default:(Var.to_exp nvar)

(** Write the provided value in the block. Mutate the state.*)
let write (s : t) (mb : Mem.block) (value : exp) : unit =
  assert (not @@ is_locked s);
  Mem.write s.mem mb value

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State register accessors } *)

(** Reset the register to a symbolic value,
    and resets the type to the provided type (or no type if not provided)*)
let reset_reg (s : t) ?(ctyp : Ctype.t option) (reg : Reg.t) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ make_reg_cell s ?ctyp reg

(** Sets the content of register *)
let set_reg (s : t) (reg : Reg.t) (tval : tval) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs reg @@ tval

(** Sets the type of the register, leaves the value unchanged *)
let set_reg_type (s : t) (reg : Reg.t) (ctyp : Ctype.t) : unit =
  assert (not @@ is_locked s);
  let { exp; _ } = Reg.Map.get s.regs reg in
  let ntval = { exp; ctyp = Some ctyp } in
  Reg.Map.set s.regs reg ntval

(** Get the content of the register *)
let get_reg (s : t) (reg : Reg.t) : tval = Reg.Map.get s.regs reg

let update_reg_exp (s : t) (reg : Reg.t) (f : exp -> exp) =
  Reg.Map.get s.regs reg |> tval_map_exp f |> Reg.Map.set s.regs reg

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pc manipulation } *)

(** Set the PC to a concrete value and keep it's type appropriate *)
let set_pc ~(pc : Reg.t) (s : t) (pcval : int) =
  let exp = Ast.Op.bits_int ~size:64 pcval in
  let ctyp = Ctype.of_frag Ctype.Global ~offset:pcval ~constexpr:true in
  set_reg s pc @@ make_tval ~ctyp exp

(** Bump a concrete PC by a concrete bump (generally the size of a non-branching instruction *)
let bump_pc ~(pc : Reg.t) (s : t) (bump : int) =
  let pc_exp = get_reg s pc |> get_exp in
  assert (ConcreteEval.is_concrete pc_exp);
  let old_pc = ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int in
  let new_pc = old_pc + bump in
  set_pc ~pc s new_pc

(** Try to evaluate the PC if it is concrete *)
let concretize_pc ~(pc : Reg.t) (s : t) =
  let pc_exp = get_reg s pc |> get_exp in
  try ConcreteEval.eval pc_exp |> Value.expect_bv |> BitVec.to_int |> set_pc ~pc s
  with ConcreteEval.Symbolic -> ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

let pp_tval { exp; ctyp } =
  let open PP in
  match ctyp with None -> pp_exp exp | Some t -> infix 2 1 colon (pp_exp exp) (Ctype.pp t)

let pp s =
  let open PP in
  record "state"
    [
      ("id", Id.pp s.id);
      ("base_state", Opt.fold ~none:!^"none" ~some:(fun s -> Id.pp s.id) s.base_state);
      ("last_pc", ptr s.last_pc);
      ("regs", Reg.Map.pp pp_tval s.regs);
      ("fenv", Fragment.Env.pp s.fenv);
      ("read_vars", Vec.ppi (fun (_, tv) -> pp_tval tv) s.read_vars);
      ("memory", Mem.pp s.mem);
      ("asserts", separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ pp_exp e) s.asserts);
    ]

(** Print only the mentioned regs and the memory and asserts since the base_state.
    Until a better solution is found, the fenv will be printed entirely all the time *)
let pp_partial ~regs s =
  let open PP in
  record "state"
    [
      ("id", Id.pp s.id);
      ("base_state", Opt.fold ~none:!^"none" ~some:(fun s -> Id.pp s.id) s.base_state);
      ("last_pc", ptr s.last_pc);
      ( "regs",
        List.map (fun reg -> (Reg.pp reg, Reg.Map.get s.regs reg |> pp_tval)) regs
        |> PP.mapping "" );
      ("fenv", Fragment.Env.pp s.fenv);
      ("read_vars", Vec.ppi (fun (_, tv) -> pp_tval tv) s.read_vars);
      ( "memory",
        s.mem.trace |> List.to_seq
        |> Seq.stop_at (function
             | Lock s' -> Opt.equal ( == ) (Some s') s.base_state
             | _ -> false)
        |> List.of_seq_rev |> list Mem.pp_event );
      ("asserts", separate_map hardline (fun e -> prefix 2 1 !^"assert:" @@ pp_exp e) s.asserts);
    ]
