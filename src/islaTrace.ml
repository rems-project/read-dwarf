(** The goal of this module is to manipulate isla_trace *)

open Isla

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type state = State.t

(** Exception that represent an Isla runtime error which should not happen *)
exception RunError of lrng * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | RunError (l, s) ->
        Some PP.(sprint @@ hardline ^^ prefix 2 1 (lrng l ^^ !^": ") (!^"RunError: " ^^ !^s))
    | _ -> None)

(** The contex of value that associate isla variable numbers to state expression *)
type value_context = State.exp hvector

(** Get the free variable using the the context and fail mentioning the location
    if the variable is not in the context *)
let get_var l vc i =
  try HashVector.get vc i
  with Invalid_argument _ ->
    raise
      (RunError
         (l, PPI.(sprintc $ !^"Could not get v" ^^ int i ^^ !^" in " ^^ hvector State.pp_sexp vc)))

(** Do a substitution assuming all free variable have a substitution,
    throw [Not_found] if one substitution is missing *)
let vc_subst_full l (vc : value_context) (exp : State.exp) : State.exp =
  (* PPI.(println @@ !^"Calling vc_subst_full " ^^ hvector sexp vc ^^ space ^^ sexp exp); *)
  let vc_subst v a =
    match v with
    | Free i -> get_var l vc i
    | State v -> Var (State v, a)
    | Bound v -> failwith "let bindings should be unfolded at this point"
  in
  IslaManip.var_subst vc_subst exp

(** This function convert a value to a state expression using the {!value_context} provided *)
let exp_of_valu l vc = function
  | Val_Symbolic i -> get_var l vc i
  | Val_Bool b -> Bool (b, l)
  | Val_Bits bv -> Bits (bv, l)
  | Val_I (bvi, i) -> Bits (IslaManip.bvi_to_bv bvi i, l)
  | Val_Enum (n, a) -> Enum ((n, a), l)
  | v -> PP.(fail $ !^"unimplemented valu_to_exp: " ^^ pp_valu v)

(** This function write an expression to a value (if it is a symbolic variable *)
let write_to_valu vc valu exp =
  match valu with Val_Symbolic i -> HashVector.add vc i exp | _ -> ()

(** This function run an isla trace on a state by mutation
    it returns the value context

    Any encountered branch are ignored and their assertion are added to the state *)
let run_trc_mut_vc ?(vc = HashVector.empty ()) (state : state) (trc : State.trc) =
  (* assert (IslaManip.is_linear trc); *)
  assert (not @@ State.is_locked state);
  let (Trace events) = trc in
  (* This function process a single event by mutating state *)
  let process : State.event -> unit = function
    | Smt (DeclareConst (_, _), _) -> ()
    | Smt (DefineConst (Free i, e), l) -> (
        try HashVector.add vc i (vc_subst_full l vc e) with RunError (_, _) -> ()
      )
    | Smt (Assert e, l) -> State.push_assert state (vc_subst_full l vc e)
    | ReadReg (name, al, valu, l) ->
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        let e : State.exp = Reg.Map.get state.regs path in
        write_to_valu vc valu (vc_subst_full l vc e)
    | WriteReg (name, al, valu, l) ->
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        (* The new expression to put in the register *)
        let new_exp : State.exp = exp_of_valu l vc valu in
        Reg.Map.set state.regs path new_exp
    | ReadMem (result, kind, addr, size, l) ->
        (* TODO stop ignoring kind *)
        let mb : State.Mem.block =
          { addr = exp_of_valu l vc addr; size = State.Mem.size_of_bytes size }
        in
        write_to_valu vc result (State.read mb state)
    | WriteMem (success, kind, addr, data, size, l) ->
        (* TODO stop ignoring kind *)
        let mb : State.Mem.block =
          { addr = exp_of_valu l vc addr; size = State.Mem.size_of_bytes size }
        in
        let data = exp_of_valu l vc data in
        State.write mb data state
    | _ -> ()
  in
  List.iter process events;
  vc

(** This function run an isla trace on a state by mutation

    Any encountered branch are ignored and their assertion are added to the state *)
let run_trc_mut (state : state) (trc : State.trc) = run_trc_mut_vc state trc |> ignore

(** This function run an isla trace on a state and return the end state as a new state

    It is just a wrapper of {!run_trc_mut} that remove the imperative interface
    The new state is fresh and locked.
*)
let run_trc start trc =
  let state = State.copy start in
  run_trc_mut state trc |> ignore;
  State.lock state;
  state
