(** This module provide utility to simplify states *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(* This is an extension of the State module *)
open State

(** Context free simplify, just expression by expression. Do it mutably *)
let ctxfree state = map_mut_exp Z3.simplify state

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
  let declared = Hashtbl.create 100 in
  let declare exp = Z3.declare_vars ~declared serv exp in
  let rec load state =
    Opt.iter load state.base_state;
    List.iter
      (fun e ->
        declare e;
        Z3.send_smt ~ppv:Var.pp serv (Ast.Op.assert_op (Z3.exp_conv e)))
      state.asserts
  in
  (* load previous assertion into Z3 *)
  Opt.iter load state.base_state;
  (* Context-free simplification of expressions *)
  (* TODO make that also context_full *)
  map_mut_exp
    (fun e ->
      if Ast.is_atomic e then e
      else begin
        declare e;
        e |> AstManip.allow_mem
        |> Z3.simplify_gen ~ppv:Var.pp ~vofs:Var.of_string
        |> AstManip.expect_no_mem
      end)
    state;
  (* Context-full simplification of assertion *)
  let found_false = ref false in
  let new_asserts =
    List.filter_map
      (fun e ->
        declare e;
        match Z3.check_gen ~ppv:Var.pp (Z3.exp_conv e) with
        | Some true -> None
        | _ -> (
            match Z3.check_sat_gen ~ppv:Var.pp (Z3.exp_conv e) with
            | Some false ->
                found_false := true;
                None
            | _ -> Some e
          ))
      state.asserts
  in
  (* If state is impossible then it has a single assertion: false *)
  if !found_false then state.asserts <- [Ast.Op.false_exp] else state.asserts <- new_asserts;
  debug "closing";
  ContextFull.closec ()
