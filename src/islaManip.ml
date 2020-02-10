(** This module provide generic manipulation function of isla ast *)

open Isla

(*****************************************************************************)
(*        Generic conversion                                                 *)
(*****************************************************************************)

(* All of those function convert the underlying state variable type through the AST *)

let var_conv_var (conv : 'a -> 'b) : 'a var -> 'b var = function
  | State a -> State (conv a)
  | Free i -> Free i

let rec exp_conv_var (conv : 'a -> 'b) : ('a, 'c) exp -> ('b, 'c) exp = function
  | Var (v, a) -> Var (var_conv_var conv v, a)
  | Bits (bv, a) -> Bits (bv, a)
  | Bool (b, a) -> Bool (b, a)
  | Unop (u, e, a) -> Unop (u, exp_conv_var conv e, a)
  | Binop (u, e, e', a) -> Binop (u, exp_conv_var conv e, exp_conv_var conv e', a)
  | Ite (c, e, e', a) -> Ite (exp_conv_var conv c, exp_conv_var conv e, exp_conv_var conv e', a)

let def_conv_var (conv : 'a -> 'b) : ('a, 'c) def -> ('b, 'c) def = function
  | DeclareConst (v, t) -> DeclareConst (var_conv_var conv v, t)
  | DefineConst (v, e) -> DefineConst (var_conv_var conv v, exp_conv_var conv e)
  | Assert e -> Assert (exp_conv_var conv e)

let event_conv_var (conv : 'a -> 'b) : ('a, 'c) event -> ('b, 'c) event = function
  | Smt (d, a) -> Smt (def_conv_var conv d, a)
  | Branch (i, s, a) -> Branch (i, s, a)
  | ReadReg (n, al, v, a) -> ReadReg (n, al, v, a)
  | WriteReg (n, al, v, a) -> WriteReg (n, al, v, a)

let trace_conv_var (conv : 'a -> 'b) (Trace (l, a)) = Trace (List.map (event_conv_var conv) l, a)

let traces_conv_var (conv : 'a -> 'b) (Traces l) = Traces (List.map (trace_conv_var conv) l)

(*****************************************************************************)
(*        Exp substitution                                                   *)
(*****************************************************************************)

(** Substitute variable with expression according to subst function *)
let rec var_subst (subst : 'v var -> 'a -> ('v, 'a) exp) (exp : ('v, 'a) exp) : ('v, 'a) exp =
  (* PPI.(println @@ !^"Calling vc_subst_full " ^^ sexp exp); *)
  let s = var_subst subst in
  match exp with
  | Var (v, a) -> subst v a
  | Binop (b, e, e', a) -> Binop (b, s e, s e', a)
  | Unop (u, e, a) -> Unop (u, s e, a)
  | Ite (c, e, e', a) -> Ite (s c, s e, s e', a)
  | e -> e

(** iterate a function on all the variable of an expression *)
let rec exp_iter_var (f : 'v var -> unit) (exp : ('v, 'a) exp) : unit =
  let i = exp_iter_var f in
  match exp with
  | Var (v, a) -> f v
  | Binop (b, e, e', a) ->
      i e;
      i e'
  | Unop (u, e, a) -> i e
  | Ite (c, e, e', a) ->
      i c;
      i e;
      i e'
  | _ -> ()

(*****************************************************************************)
(*        Accessor list conversion                                           *)
(*****************************************************************************)

(* TODO make that transparent by changing isla_lang *)

let accessor_of_string s = Field s

let string_of_accessor (Field s) = s

let accessor_of_string_list = function [] -> Nil | l -> Cons (List.map accessor_of_string l)

let string_of_accessor_list = function Nil -> [] | Cons l -> List.map string_of_accessor l

(*****************************************************************************)
(*        valu string path access                                            *)
(*****************************************************************************)

let rec valu_get valu path =
  match (valu, path) with
  | (_, []) -> valu
  | (Val_Struct s, a :: l) -> valu_get (List.assoc a s) l
  | _ -> failwith "islaManip.valu_get: Invalid path in IslaManip.valu_get"

(*****************************************************************************)
(*         bvi conversion                                                    *)
(*****************************************************************************)

(** Convert the bvi constant style like (_ bv42 6) to explicit style #b101010 *)
let bvi_to_bv (bvi : bvi) (size : int) =
  if bvi > 0 then
    if size mod 4 = 0 then
      let s = Printf.sprintf "%x" bvi in
      "#x" ^ String.make ((size / 4) - String.length s) '0' ^ s
    else failwith "TODO in IslaManip.bvi_to_bv"
  else begin
    assert (bvi = -1);
    if size mod 4 = 0 then "#x" ^ String.make (size / 4) 'F' else "#b" ^ String.make size '1'
  end
