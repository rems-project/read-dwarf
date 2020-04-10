(** This module introduce a type to represent the state of the machine.

    Look at {!t} for more details
 *)

open Logs.Logger (struct
  let str = "State"
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Support types } *)

type 'a vector = 'a Vector.t

(** This type is the standard annotation inside the state stucture.
    For now it is Isla.lrng, but it may change depending on our needs *)
type annot = Isla.lrng

let dummy_annot = Isla.UnknownRng

type ty = Isla.ty

module Id = struct
  type t = int

  let to_string = string_of_int

  let of_string = int_of_string

  let pp id = id |> to_string |> PP.string
end

type id = Id.t

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State types } *)

(** All the possible atomic access sizes *)
type mem_size = B8 | B16 | B32 | B64 | B128

(** Represent the state of the machine.
    Should only be mutated when created before locking.

    The {!lock} function allow to lock a state for mutation. This is not encoded into
    the type system (yet) but it should nevertheless be respected. Once a state is locked,
    it should not be mutated anymore.
*)
type t = {
  id : id;
  mutable regs : tval Reg.Map.t;
  read_vars : (ty * tval) vector;
  mutable asserts : exp list;
  mem : mem;
  (* elf_file : file option; optionally elf file so that
     state manipulation dependent on elf things would still work without having to pass
     the elf file around *)
  fenv : Fragment.env;
}

and tval = { ctyp : Ctype.t option; exp : exp }

and exp = (svar, annot) Isla.exp

and svar = Register of t * Reg.path | ReadVar of t * int | Arg of int | RetArg

and mem_block = { addr : exp; size : mem_size }

and mem_event =
  | Read of mem_block * svar
  | Write of mem_block * exp
  | Lock of t
      (** represents that all the memory operation below were already in the given state *)

and mem = { mutable trace : mem_event list }

(* Other isla aliases *)

type var = svar Isla.var

type event = (svar, annot) Isla.event

type trc = (svar, annot) Isla.trc

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

  type t = svar

  let to_string = function
    | Register (state, path) ->
        Printf.sprintf "%s:%s" (state |> to_id |> Id.to_string) (Reg.path_to_string path)
    | ReadVar (state, num) -> Printf.sprintf "%s:r%i" (state |> to_id |> Id.to_string) num
    | Arg num -> Printf.sprintf "arg%i" num
    | RetArg -> "retarg"

  let of_string s : t =
    let fail () = Raise.inv_arg "Invalid state variable: %s" s in
    match String.split_on_char ':' s with
    | [state; var] -> (
        let state : state = state |> Id.of_string |> of_id in
        match var.[0] with
        | 'r' -> ReadVar (state, int_of_string @@ String.sub var 1 (String.length var - 1))
        | _ -> Register (state, Reg.path_of_string var)
      )
    | [var] -> (
        match var.[0] with
        | 'a' -> Arg (Scanf.sscanf var "arg%d" Fun.id)
        | 'r' -> if var != "retarg" then fail () else RetArg
        | _ -> fail ()
      )
    | _ -> fail ()

  let to_exp (v : t) : exp = Isla.Var (Isla.State v, dummy_annot)

  let pp sv = sv |> to_string |> PP.string
end

let pp_sexp exp = Isla_lang.PP.pp_exp Var.pp exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 State memory management } *)

(** This module helps managing the memory part of the state. *)
module Mem = struct
  type size = mem_size

  let size_to_bv : mem_size -> ty = function
    | B8 -> Ty_BitVec 8
    | B16 -> Ty_BitVec 16
    | B32 -> Ty_BitVec 32
    | B64 -> Ty_BitVec 64
    | B128 -> Ty_BitVec 128

  type block = mem_block

  type event = mem_event

  type t = mem

  let empty () = { trace = [] }

  let make state = { trace = [] }

  let read mem mb svar = mem.trace <- Read (mb, svar) :: mem.trace

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

  let size_to_bytes = function B8 -> 1 | B16 -> 2 | B32 -> 4 | B64 -> 8 | B128 -> 16

  let size_to_bits size = 8 * size_to_bytes size

  let size_of_bytes = function
    | 1 -> B8
    | 2 -> B16
    | 4 -> B32
    | 8 -> B64
    | 16 -> B128
    | d ->
        failwith
          (Printf.sprintf "size_of_bytes: %d bytes is not a supported size for memory access" d)

  let pp_size size = PP.(dprintf "%dbits" (size_to_bits size))

  let pp_block block =
    PP.(pp_size block.size ^^ surround 2 0 lbracket (pp_sexp block.addr) rbracket)

  let pp_event : event -> PP.document = function
    | Lock s -> PP.(dprintf "Lock %d" s.id)
    | Read (mb, svar) -> PP.(!^"Reading " ^^ Var.pp svar ^^ !^" from: " ^^ pp_block mb)
    | Write (mb, exp) -> PP.(!^"Writing " ^^ pp_sexp exp ^^ !^" at " ^^ pp_block mb)

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

let make_reg_cell state ?ctyp p = { exp = Var.to_exp (Register (state, p)); ctyp }

(** Makes a fresh state with all variable fresh and new

    Remark: This create a state according to the current register layout in the Reg module
    ({!Reg.index}). If new registers are added afterwards, this state won't have them.
    Use {!copy_extend} to fix that.
*)
let make () =
  let id = !next_id in
  let state =
    {
      id;
      regs = Reg.Map.dummy ();
      read_vars = Vector.empty ();
      asserts = [];
      mem = Mem.empty ();
      fenv = Fragment.Env.make ();
    }
  in
  (* Warning: here I'm creating a cyclic reference. Please be aware of that *)
  state.regs <- Reg.Map.init @@ make_reg_cell state;
  next_id := id + 1;
  WeakMap.add id2state id state;
  state

(** Make a state using the argument passed *)
let make_of regs read_vars asserts mem fenv =
  let id = !next_id in
  let state = { id; regs; read_vars; asserts; mem; fenv } in
  next_id := id + 1;
  WeakMap.add id2state id state;
  state

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retro-action.

    Remark: This create a new state with the same register layout as the input state
    It will not accommodate register discovered in between
    Use {!copy_extend} to add the new register that are missing
*)
let copy state =
  let id = !next_id in
  let nstate =
    {
      id;
      regs = Reg.Map.copy state.regs;
      read_vars = Vector.empty ();
      asserts = state.asserts;
      mem = Mem.copy state.mem;
      fenv = Fragment.Env.copy state.fenv;
    }
  in
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

(** Add all missing register that are not present as fresh new variables
*)
let extend_mut state =
  assert (not @@ is_locked state);
  state.regs <- Reg.Map.copy_extend ~init:(make_reg_cell state) state.regs

(** Do a deep copy of all the mutable part of the state,
    so it can be mutated without retro-action.
    All new register that are not present are added with fresh new variables
    as if created by {!make}
 *)
let copy_extend state =
  let id = !next_id in
  let nstate =
    {
      id;
      regs = Reg.Map.dummy ();
      read_vars = Vector.empty ();
      asserts = state.asserts;
      mem = Mem.copy state.mem;
      fenv = Fragment.Env.copy state.fenv;
    }
  in
  nstate.regs <- Reg.Map.copy_extend ~init:(make_reg_cell state) state.regs;
  next_id := id + 1;
  WeakMap.add id2state id nstate;
  nstate

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
  Reg.Map.map_mut tval_map s.regs;
  Vector.map_mut (Pair.map Fun.id tval_map) s.read_vars;
  s.asserts <- List.map f s.asserts;
  Mem.map_mut_exp f s.mem

let map_exp (f : exp -> exp) s : t =
  let res = copy s in
  map_mut_exp f res;
  res

(** Iterates a function on all the expressions of a state *)
let iter_exp (f : exp -> unit) s =
  let tval_iter tv = f tv.exp in
  Reg.Map.iter tval_iter s.regs;
  Vector.iter (Pair.iter ignore tval_iter) s.read_vars;
  List.iter f s.asserts;
  Mem.iter_exp f s.mem

(** Iterates a function on all the variables of a state *)
let iter_var (f : var -> unit) s = iter_exp (IslaManip.exp_iter_var f) s

(** Return the type of a state variable *)
let svar_type var =
  match var with
  | Register (state, p) -> p |> Reg.path_type |> Reg.assert_plain
  | ReadVar (state, i) -> Vector.get state.read_vars i |> fst
  | Arg _ -> Isla.Ty_BitVec 64
  | RetArg -> Isla.Ty_BitVec 64

(** Create a new Extra symbolic variable by mutating the state *)
let make_extra (ty : ty) ?ctyp (s : t) : svar =
  assert (not @@ is_locked s);
  let len = Vector.length s.read_vars in
  let svar = ReadVar (s, len) in
  Vector.add_one s.read_vars (ty, { ctyp; exp = Var.to_exp svar });
  svar

(** Read the block designated by mb from the state and return an expression read.
    This may mutate the state to add the read of the state as

    Depending on the inputs and the setting, this expression could be:
    - a symbolic Extra variable that has a read event in the trace
    - a concrete constant (read from rodata)
    - a symbolic initial memory variable (when and if those exist)

    In practice for now it is always the first case
*)
let read (mb : Mem.block) (s : t) : tval =
  assert (not @@ is_locked s);
  let ty = Mem.size_to_bv mb.size in
  let nvar = make_extra ty s in
  Mem.read s.mem mb nvar;
  { ctyp = None; exp = Var.to_exp nvar }

(** Write the provided value in the block. Mutate the state.*)
let write (mb : Mem.block) (value : exp) (s : t) : unit =
  assert (not @@ is_locked s);
  Mem.write s.mem mb value

(** Reset the register in given path to a symbolic value *)
let reset_reg (path : Reg.path) ?(ctyp : Ctype.t option) (s : t) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs path @@ make_reg_cell s ?ctyp path

(** Sets the content of register *)
let set_reg (path : Reg.path) ?(ctyp : Ctype.t option) (exp : exp) (s : t) : unit =
  assert (not @@ is_locked s);
  Reg.Map.set s.regs path @@ { exp; ctyp }

(** Sets the type of the register, leaves the value unchanged *)
let set_reg_type (path : Reg.path) (ctyp : Ctype.t) (s : t) : unit =
  assert (not @@ is_locked s);
  let { exp; _ } = Reg.Map.get s.regs path in
  let ntval = { exp; ctyp = Some ctyp } in
  Reg.Map.set s.regs path ntval

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty printing } *)

let pp_trc trc = Isla_lang.PP.pp_trc Var.pp trc

let pp_tval { exp; ctyp } =
  let open PP in
  match ctyp with None -> pp_sexp exp | Some t -> infix 2 1 colon (pp_sexp exp) (Ctype.pp t)

let pp s =
  PP.(
    record "state"
      [
        ("id", Id.pp s.id);
        ("regs", Reg.Map.pp pp_tval s.regs);
        ("read_vars", Vector.ppi (fun (_, tv) -> pp_tval tv) s.read_vars);
        ( "asserts",
          s.asserts |> List.map (fun e -> prefix 2 1 !^"assert:" $ pp_sexp e) |> separate hardline
        );
        ("memory", Mem.pp s.mem);
        ("fenv", Fragment.Env.pp s.fenv);
      ])
