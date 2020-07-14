(** This module provide utility to simplify states *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(* This is an extension of the State module *)
open State
module Z3St = Z3.Make (Var)

(** Context free simplify, just expression by expression. Do it mutably *)
let ctxfree state = map_mut_exp Z3St.simplify_full state

(** Do a context aware simplify, for now, it just does a context free simplify and then
    remove assertion that proven true or false by Z3
*)
module ContextFull = Z3.ContextCounter (struct
  let str = "State context-full simplification"
end)

(** Do a context aware simplify, for now, it just does a context free simplify and then
    remove assertion that proven true or false by Z3.

    If a state has a false assertion anywhere then all assertions will collapse
    in a single false.
*)
let ctxfull state =
  debug "StateSimplifying %d" state.id;
  let serv = Z3.ensure_started_get () in
  ContextFull.openc ();
  let declared = Z3St.Htbl.create 100 in
  let declare exp = Z3St.declare_vars ~declared serv exp in
  let rec load state =
    Opt.iter load state.base_state;
    List.iter (Z3St.send_assert_decl ~declared serv) state.asserts
  in
  (* load previous assertion into Z3 *)
  Opt.iter load state.base_state;
  (* Context-free simplification of expressions *)
  (* TODO make that also context_full *)
  map_mut_exp (fun e -> if Ast.is_atomic e then e else Z3St.simplify_decl ~declared serv e) state;
  (* Context-full simplification of assertion *)
  let found_false = ref false in
  let new_asserts =
    List.filter_map
      (fun e ->
        declare e;
        match Z3St.check_both serv e with
        | Some true -> None
        | Some false ->
            found_false := true;
            None
        | None -> Some e)
      state.asserts
  in
  (* If state is impossible then it has a single assertion: false *)
  if !found_false then state.asserts <- [ExpTyped.false_] else state.asserts <- new_asserts;
  debug "closing";
  ContextFull.closec ()
