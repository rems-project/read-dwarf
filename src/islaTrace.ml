(** The goal of this module is to manipulate isla_trace *)

open Isla

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type state = State.t

(** Raised when a branch is found in a trace that was supposed to be linear *)
exception Branch

(** The contex of value that associate isla variable numbers to state expression *)
type value_context = State.exp hvector

(** Do a substitution assuming all free variable have a substitution,
    throw [Not_found] if one substitution is missing *)
let vc_subst_full (vc : value_context) (exp : State.exp) : State.exp =
  (* PPI.(println @@ !^"Calling vc_subst_full " ^^ hvector sexp vc ^^ space ^^ sexp exp); *)
  let vc_subst v a = match v with Free i -> HashVector.get vc i | State v -> Var (State v, a) in
  IslaManip.var_subst vc_subst exp

(** This function run an isla trace on a state and return the end state.*)
let run_trc (init_state : state) (trace : State.trc) =
  assert (IslaManip.is_linear trace);
  let vc : value_context = HashVector.empty () in
  let state = State.copy init_state in
  let (Trace events) = trace in
  (* This function process a single event by mutating state *)
  let process : State.event -> unit = function
    | Smt (DeclareConst (_, _), _) -> ()
    | Smt (DefineConst (Free i, e), _) -> HashVector.add vc i (vc_subst_full vc e)
    | Smt (Assert e, _) -> State.push_assert state (vc_subst_full vc e)
    | ReadReg (name, al, valu, _) -> (
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        let e : State.exp = Reg.Map.get state.regs path in
        (* If a read, reads something else than a symbolic variable, nothing happens *)
        match valu with Val_Symbolic i -> HashVector.add vc i (vc_subst_full vc e) | _ -> ()
      )
    | WriteReg (name, al, valu, l) ->
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        (* The new expression to put in the register *)
        let new_exp =
          match valu with
          | Val_Symbolic i -> HashVector.get vc i
          | Val_Bool b -> Bool (b, l)
          | Val_Bits bv -> Bits (bv, l)
          | Val_I (bvi, i) -> Bits (IslaManip.bvi_to_bv bvi i, l)
          | _ -> failwith "unimplemented writing"
        in
        Reg.Map.set state.regs path new_exp
    | _ -> ()
  in
  List.iter process events;
  PPI.(println $ hvector sexp vc);
  state
