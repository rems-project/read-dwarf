(** This module is about preprocessing isla traces. This includes:

    - Removing exceptional traces
    - Removing useless garbage and simplifying
    - Removing all effect of initialization before cycle
    - Splitting branches that are non exceptional the right way

    Once preprocessed, there is only a list of traces.

    - Special instructions like smc have zero traces and must be provided with special semantics
    - Normal instructions have one traces (all exceptional cases are classified as UB)
    - Branching instruction have more than one traces.

    For example at time of writing,
    a basic load like "ldr x0, [x1]" has about 1300 variable before preprocessing and
    27 after.

    TODO: Remove useless register reads (Need a model where reading register has no side effect).
    TODO: Apply context-free Z3 simplification to expressions
    TODO: Apply context-full removal of useless assertion (often the same assertion twice or more).
*)

open Isla

open Logs.Logger (struct
  let str = __MODULE__
end)

type simplify_context_element =
  | Declared of { ty : ty; loc : lrng }
  | Defined of { exp : rexp; loc : lrng }
  | Processed of int

let expect_processed = function
  | Processed i -> i
  | _ -> Raise.fail "Variables should be processed at this point"

let simplify_trc (Trace events : rtrc) =
  (* Phase 1: Discover which variable are actually used *)
  let used = HashVector.empty () in
  let process_used event =
    let rec process_used_valu = function
      | Val_Symbolic i ->
          debug "Marking v%d as used" i;
          HashVector.set used i ()
      | v -> IslaManip.direct_valu_iter_valu process_used_valu v
    in
    IslaManip.event_iter_valu process_used_valu event
  in
  List.iter process_used events;
  debug "The list of used vars is:\n%t" (PP.top (HashVector.pp PP.erase) used);
  (* Phase 2: Only keep variables that are actually used
     This is done lazily. On declaration or definition, variables are
     Stored in a lazy fashion in the context.
     If they are used as determined by the previous phase, they are immediately declared.
     When an expression is required, all defined variable in the expression
     not yet commited are inlined.
     Variables are also renumbered at the same time. *)
  let simplify_context = HashVector.empty () in
  let new_variables = Counter.make 0 in
  let res = ref [] in
  let push_event (d : revent) = res := d :: !res in
  let push_smt loc (d : rsmt) = push_event (Smt (d, loc)) in
  (* Commits the variable i.e output its declaration/definition in the trace,
     and return the new value *)
  let rec commit i =
    match HashVector.get simplify_context i with
    | Declared { ty; loc } ->
        debug "Commiting declared variable %d" i;
        let new_val = Counter.get new_variables in
        HashVector.set simplify_context i (Processed new_val);
        push_smt loc (DeclareConst (new_val, ty));
        new_val
    | Defined { exp; loc } ->
        debug "Commiting defined variable %d" i;
        let new_val = Counter.get new_variables in
        HashVector.set simplify_context i (Processed new_val);
        debug "New id is %d" new_val;
        let new_exp = simplify_exp exp in
        debug "New exp is %t" PP.(fun o -> fprint o $ pp_exp new_exp);
        push_smt loc (DefineConst (new_val, new_exp));
        new_val
    | Processed v -> v
  (* Simplify and expression, by inlining all not committed defined variable and
     committing all required declared variables
  *)
  and simplify_exp (e : rexp) : rexp =
    let simplify_var v loc : rexp =
      match HashVector.get simplify_context v with
      | Declared _ ->
          debug "Simplifying declared variable %d" v;
          let new_v = commit v in
          Var (new_v, loc)
      | Processed new_v -> Var (new_v, loc)
      | Defined { exp; _ } ->
          debug "Simplifying defined variable %d with exp %t" v (PP.top pp_exp exp);
          simplify_exp exp
    in
    IslaManip.var_subst simplify_var e
  in
  let rec simplify_valu = function
    | Val_Symbolic i -> Val_Symbolic (i |> HashVector.get simplify_context |> expect_processed)
    | valu -> IslaManip.direct_valu_map_valu simplify_valu valu
  in
  let simplify_event = function
    | Smt (DeclareConst (v, ty), loc) ->
        debug "Declaring v%d with type %t" v (PP.top pp_ty ty);
        HashVector.set simplify_context v (Declared { ty; loc });
        if HashVector.mem used v then commit v |> ignore
    | Smt (DefineConst (v, exp), loc) ->
        debug "Defining v%d with value %t" v (PP.top pp_exp exp);
        HashVector.set simplify_context v (Defined { exp; loc });
        if HashVector.mem used v then commit v |> ignore
    | Smt (Assert exp, loc) ->
        let nexp = simplify_exp exp in
        debug "Asserting old %t new %t" (PP.top pp_exp exp) (PP.top pp_exp nexp);
        push_event @@ Smt (Assert nexp, loc)
    | event ->
        debug "Handling event %t" (PP.top pp_event event);
        push_event @@ IslaManip.direct_event_map_valu simplify_valu event
  in
  List.iter simplify_event events;
  debug "Nearly finished simplify_trc with %d variables, now reversing"
    (1 + Counter.read new_variables);
  Trace (List.rev !res)

(** Preprocess a set of traces. *)
let preprocess trcs =
  let preprocess_one (b, trc) =
    if not b then None
    else
      let trc = IslaManip.filter trc in
      let trc = simplify_trc trc in
      Some trc
  in
  List.filter_map preprocess_one trcs
