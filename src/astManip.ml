(* This file use Obj.magic a lot on the AST. No one else is allowed to. On any change to the AST types, all the uses of Obj.magic must be checked again *)

open Ast

open Logs.Logger (struct
  let str = "AstManip"
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Non-recursive maps and iters }

   This section is filled on demand.

   [direct_a_map_b] take a function [b -> b] and applies it to all [b] in [a], non-recursively.
   Then a new a with the same structure is formed.

   [direct_a_iter_b] take a function [b -> unit] and applies it to all [b] in [a], non-recursively.
*)

let direct_exp_map_exp (f : ('a, 'v, 'b, 'p, 'm) exp -> ('a, 'v, 'b, 'p, 'm) exp) = function
  | Unop (u, e, l) -> Unop (u, f e, l)
  | Binop (b, e, e', l) -> Binop (b, f e, f e', l)
  | Manyop (m, el, l) -> Manyop (m, List.map f el, l)
  | Ite (c, e, e', l) -> Ite (f c, f e, f e', l)
  | Let (b, e, e', l) -> Let (b, f e, f e', l)
  | Bound _ as b -> b
  | Bits _ as b -> b
  | Bool _ as b -> b
  | Enum _ as e -> e
  | Var _ as v -> v

let direct_exp_iter_exp (i : ('a, 'v, 'b, 'p, 'm) exp -> unit) = function
  | Unop (u, e, l) -> i e
  | Binop (b, e, e', l) ->
      i e;
      i e'
  | Manyop (m, el, l) -> List.iter i el
  | Ite (c, e, e', l) ->
      i c;
      i e;
      i e'
  | Let (b, e, e', l) ->
      i e;
      i e'
  | Bits (bv, a) -> ()
  | Bool (b, a) -> ()
  | Enum (e, a) -> ()
  | Var (v, a) -> ()
  | Bound (b, a) -> ()

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Recursive maps and iters }

    This section is filled on demand.

    [a_map_b] take a function [b -> b] and applies it to all the [b] in [a], and do that
    recursively on all b that appear directly or indirectly in a

    [a_iter_b] take a function [b -> unit] and applies it to all the [b] in [a], and do that
    recursively

    Doing this when a = b is not well defined, and can be easily done using the direct
    version from previous section.

*)

(** iterate a function on all the variable of an expression *)
let rec exp_iter_var (f : 'v -> unit) : ('a, 'v, 'b, 'p, 'm) exp -> unit = function
  | Var (v, a) -> f v
  | exp -> direct_exp_iter_exp (exp_iter_var f) exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Variable type conversion }

    All of those function convert the underlying variable type through the AST.
    They cannot be the usual map function because they change the type
*)

let rec exp_conv_var (conv : 'va -> 'vb) (exp : ('a, 'va, 'b, 'p, 'm) exp) :
    ('a, 'vb', 'b, 'p, 'm) exp =
  let ec = exp_conv_var conv in
  match exp with
  | Var (v, a) -> Var (conv v, a)
  | Bound _ as b -> b
  | Bits _ as b -> b
  | Bool _ as b -> b
  | Enum _ as e -> e
  | Unop (u, e, a) -> Unop (u, ec e, a)
  | Binop (u, e, e', a) -> Binop (u, ec e, ec e', a)
  | Manyop (m, el, a) -> Manyop (m, List.map ec el, a)
  | Ite (c, e, e', a) -> Ite (ec c, ec e, ec e', a)
  | Let (v, e, e', a) -> Let (v, ec e, ec e', a)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bound variables and let-bindings } *)

(** Allow bound variables and lets in an expression. *)
let allow_lets : ('a, 'v, no, 'p, 'm) exp -> ('a, 'v, 'b, 'p, 'm) exp = Obj.magic

let smt_allow_lets : ('a, 'v, no, 'p, 'm) smt -> ('a, 'v, 'b, 'p, 'm) smt = Obj.magic

(** Unfold all lets. The output type still allow lets so that the output of this function can still be used without {!allow_bound} *)
let rec unfold_lets ?(context = Hashtbl.create 5) (exp : ('a, 'v, 'b1, 'p, 'm) exp) :
    ('a, 'v, 'b2, 'p, 'm) exp =
  let ul = unfold_lets ~context in
  match exp with
  | Bound (b, l) -> Hashtbl.find context b
  | Let (b, e1, e2, l) ->
      let e1 = ul e1 in
      Hashtbl.add context b e1;
      let res = ul e2 in
      Hashtbl.remove context b;
      res
  | Bits _ as b -> b
  | Bool _ as b -> b
  | Enum _ as e -> e
  | Var _ as v -> v
  | Unop (u, e, l) -> Unop (u, ul e, l)
  | Binop (b, e, e', l) -> Binop (b, ul e, ul e', l)
  | Manyop (m, el, l) -> Manyop (m, List.map ul el, l)
  | Ite (c, e, e', l) -> Ite (ul c, ul e, ul e', l)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Memory operation } *)

(** Allow memory operations in an expression. *)
let allow_mem : ('a, 'v, 'b, 'p, no) exp -> ('a, 'v, 'b, 'p, 'm) exp = Obj.magic

let smt_allow_mem : ('a, 'v, 'b, 'p, no) smt -> ('a, 'v, 'b, 'p, 'm) smt = Obj.magic

(** Allow memory operations in a type. *)
let ty_allow_mem : no ty -> 'm ty = Obj.magic

let check_no_mem (e : ('a, 'v, 'b, 'p, 'm) exp) : bool =
  let ref_res = ref true in
  let rec check = function
    | Binop (Binmem _, _, _, _) -> ref_res := false
    | e -> direct_exp_iter_exp check e
  in
  check e;
  !ref_res

let expect_no_mem ?(handler = fun () -> failwith "Expected no mem") :
    ('a, 'v, 'b, 'p, 'm1) exp -> ('a, 'v, 'b, 'p, 'm2) exp =
 fun exp -> if check_no_mem exp then Obj.magic exp else handler ()
