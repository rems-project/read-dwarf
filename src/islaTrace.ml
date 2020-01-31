(** The goal of this module is to manipulate isla_trace *)

open Isla

type 'a vector = 'a Vector.t

type 'a hvector = 'a HashVector.t

type state = State.t

(** Check if a trace is linear (has no branches) *)
let is_linear (Trace (events, _) : ('v, 'a) trc) =
  let rec no_branch = function
    | [] -> true
    | Branch (_, _, _) :: _ -> false
    | _ :: l -> no_branch l
  in
  no_branch events

(** Check if a group of trace is linear (has no branches and only one trace) *)
let is_linear_s : ('v, 'a) term -> bool = function
  | Traces (_ :: _ :: _) -> false
  | Traces [t] -> is_linear t
  | Traces [] -> true

(** raise when a branch is found in a trace that was supposed to be linear *)
exception Branch

type value_context = State.exp hvector

(** Do a substitution assuming all free variable have a substitution, throw Not_found if one substitution is missing *)
let vc_subst_full (vc : value_context) (exp : State.exp) : State.exp =
  (* PPI.(println @@ !^"Calling vc_subst_full " ^^ hvector sexp vc ^^ space ^^ sexp exp); *)
  let vc_subst v a = match v with Free i -> HashVector.get vc i | State v -> Var (State v, a) in
  IslaManip.var_subst vc_subst exp

(** This function run an isla trace on a state and return the end_state.
*)
let run_lin_trace (init_state : state) (trace : State.trc) =
  assert (is_linear trace);
  (* type context *)
  let vc : value_context = HashVector.empty () in
  (* value_context *)
  let state = State.copy init_state in
  let (Trace (events, _)) = trace in
  let process : State.event -> unit = function
    | Smt (DeclareConst (_, _), _) -> ()
    | Smt (DefineConst (Free i, e), _) -> HashVector.add vc i (vc_subst_full vc e)
    | Smt (Assert e, _) -> State.push_assert state (vc_subst_full vc e)
    | ReadReg (name, al, valu, _) -> (
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        let e : State.exp = Reg.Map.get state.regs path in
        match valu with Val_Symbolic i -> HashVector.add vc i (vc_subst_full vc e) | _ -> ()
      )
    | WriteReg (name, al, valu, l) ->
        print_endline @@ "writing " ^ name;
        let string_path = IslaManip.string_of_accessor_list al in
        let valu = IslaManip.valu_get valu string_path in
        let path = Reg.path_of_string_list (name :: string_path) in
        let ne =
          match valu with
          | Val_Symbolic i -> HashVector.get vc i
          | Val_Bool b -> Bool (b, l)
          | Val_Bits bv -> Bits (bv, l)
          | Val_I (bvi, i) -> Bits (IslaManip.bvi_to_bv bvi i, l)
          | _ -> failwith "unimplemented writing"
        in
        Reg.Map.set state.regs path ne
    | _ -> ()
  in
  List.iter process events;
  PPI.(println @@ hvector sexp vc);
  state
