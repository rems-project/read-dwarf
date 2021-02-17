(** This module provide generic facilities of expression and SMT statements
    provided by {!Ast}. It is intended to only provide syntactic facilities over
    {!Ast} types, in particular {!Ast.exp}.

    In particular it provides generic mapping and iteration function over
    expressions as well a function allowing to convert between the various
    {!SymbolicExpression.typopt}.

    Warning: due to Ocaml type system limitations mainly
    {{:https://github.com/ocaml/ocaml/issues/9459} issue [#9456]},
    this module is sometime required to use {!Obj.magic} in some specific cases.
    No other module should ever do that.
    If you need to use [Obj.magic] to bypass Ocaml type system limitation about
    {!Ast} type, add a function here instead.

*)

open Base

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Get annotations } *)

(** Get the annotation of an expression

    TODO: This would be much more efficient if the annotation
    was always the first member of the constructor and not the last
    (in that case the offset to fetch the annotation do not depend on
    the constructor index). This may require to modify ott.
*)
let annot_exp : ('a, 'v, 'b, 'm) exp -> 'a = function
  | Var (_, a) -> a
  | Bound (_, a) -> a
  | Bits (_, a) -> a
  | Bool (_, a) -> a
  | Enum (_, a) -> a
  | Unop (_, _, a) -> a
  | Binop (_, _, _, a) -> a
  | Manyop (_, _, a) -> a
  | Ite (_, _, _, a) -> a
  | Let (_, _, _, a) -> a

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Non-recursive maps and iters }

   This section is filled on demand.

   [direct_a_map_b] take a function [b -> b] and applies it to all [b] in [a], non-recursively.
   Then a new a with the same structure is formed.

   [direct_a_iter_b] take a function [b -> unit] and applies it to all [b] in [a], non-recursively.
*)

let direct_exp_map_exp (f : ('a, 'v, 'b, 'm) exp -> ('a, 'v, 'b, 'm) exp) = function
  | Unop (u, e, l) -> Unop (u, f e, l)
  | Binop (b, e, e', l) -> Binop (b, f e, f e', l)
  | Manyop (m, el, l) -> Manyop (m, List.map f el, l)
  | Ite (c, e, e', l) -> Ite (f c, f e, f e', l)
  | Let (b, bs, e, l) -> Let (Pair.map Fun.id f b, List.map (Pair.map Fun.id f) bs, f e, l)
  | Bound _ as b -> b
  | Bits _ as b -> b
  | Bool _ as b -> b
  | Enum _ as e -> e
  | Var _ as v -> v

let direct_exp_iter_exp (i : ('a, 'v, 'b, 'm) exp -> unit) = function
  | Unop (_, e, _) -> i e
  | Binop (_, e, e', _) ->
      i e;
      i e'
  | Manyop (_, el, _) -> List.iter i el
  | Ite (c, e, e', _) ->
      i c;
      i e;
      i e'
  | Let (b, bs, e, _) ->
      i (snd b);
      List.iter (Pair.iter ignore i) bs;
      i e
  | Bits _ -> ()
  | Bool _ -> ()
  | Enum _ -> ()
  | Var _ -> ()
  | Bound _ -> ()

let direct_exp_fold_left_exp (f : 'a -> _ exp -> 'a) (v : 'a) = function
  | Unop (_, e, _) -> f v e
  | Binop (_, e, e', _) -> f (f v e) e'
  | Manyop (_, el, _) -> List.fold_left f v el
  | Ite (c, e, e', _) -> f (f (f v c) e) e'
  | Let (b, bs, e, _) -> f (List.fold_left (fun v (_, e) -> f v e) (f v (snd b)) bs) e
  | Bits _ -> v
  | Bool _ -> v
  | Enum _ -> v
  | Var _ -> v
  | Bound _ -> v

let direct_exp_for_all_exp (p : _ exp -> bool) exp =
  direct_exp_fold_left_exp (fun b e -> b && p e) true exp

let direct_exp_exists_exp (p : _ exp -> bool) exp =
  direct_exp_fold_left_exp (fun b e -> b || p e) false exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Recursive maps and iters }

    This section is filled on demand.

    [a_map_b] take a function [b -> b] and applies it to all the [b] in [a], and do that
    recursively on all b that appear directly or indirectly in a

    [a_iter_b] take a function [b -> unit] and applies it to all the [b] in [a], and do that
    recursively

    Doing this when [a = b] is not well defined, and can be easily done using the direct
    version from previous section. *)

(** Iterate a function on all the variable of an expression *)
let rec exp_iter_var (f : 'v -> unit) : ('a, 'v, 'b, 'm) exp -> unit = function
  | Var (v, _) -> f v
  | exp -> direct_exp_iter_exp (exp_iter_var f) exp

let rec exp_map_var (conv : 'va -> 'vb) (exp : ('a, 'va, 'b, 'm) exp) : ('a, 'vb', 'b, 'm) exp =
  let ec = exp_map_var conv in
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
  | Let (b, bs, e, l) -> Let (Pair.map Fun.id ec b, List.map (Pair.map Fun.id ec) bs, ec e, l)

(** Iterate a function on all the annotations of an expression *)
let rec exp_iter_annot (f : 'a -> unit) (exp : ('a, 'v, 'b, 'm) exp) : unit =
  f (annot_exp exp);
  direct_exp_iter_exp (exp_iter_annot f) exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Variable type conversion }

    All of those function convert the underlying variable type through the AST.
    They cannot be the usual map function because they change the type
*)

(** Old alias to make conversion explicit *)
let exp_conv_var = exp_map_var

(** Substitute variable with expression according to substitution function *)
let rec exp_var_subst (subst : 'va -> 'a -> ('a, 'vb, 'b, 'm) exp) (exp : ('a, 'va, 'b, 'm) exp) :
    ('a, 'vb, 'b, 'm) exp =
  let es = exp_var_subst subst in
  match exp with
  | Var (v, a) -> subst v a
  | Bound _ as b -> b
  | Bits _ as b -> b
  | Bool _ as b -> b
  | Enum _ as e -> e
  | Unop (u, e, a) -> Unop (u, es e, a)
  | Binop (u, e, e', a) -> Binop (u, es e, es e', a)
  | Manyop (m, el, a) -> Manyop (m, List.map es el, a)
  | Ite (c, e, e', a) -> Ite (es c, es e, es e', a)
  | Let (b, bs, e, l) -> Let (Pair.map Fun.id es b, List.map (Pair.map Fun.id es) bs, es e, l)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Bound variables and let-bindings }

    This section allow to go from expression without let-bindings
    to expression with them and vice-versa.*)

(** Allow bound variables and lets in an expression.
    This operation is a no-op and has no runtime cost, it's just a type change.*)
let allow_lets : ('a, 'v, no, 'm) exp -> ('a, 'v, 'b, 'm) exp =
  (* Check that exp is covariant in the 'b parameter, otherwise this is unsound *)
  let module M : sig
    type +'b t [@@ocaml.warning "-34"] (* unused type *)
  end = struct
    type 'b t = (no, no, 'b, no) exp
  end in
  Obj.magic

(** Same as {!allow_lets} but for the {!smt} type *)
let smt_allow_lets : ('a, 'v, no, 'm) smt -> ('a, 'v, 'b, 'm) smt =
  (* Check that smt is covariant in the 'b parameter, otherwise this is unsound *)
  let module M : sig
    type +'b t [@@ocaml.warning "-34"] (* unused type *)
  end = struct
    type 'b t = (no, no, 'b, no) smt
  end in
  Obj.magic

(** Unfold all lets. There are no remaining lets in the output,
    Therefore the output type of let binding can be anything including {!Ast.no}.
    In particular doing {!allow_lets} after this function is useless *)
let rec unfold_lets ?(context = Hashtbl.create 5) (exp : ('a, 'v, 'b1, 'm) exp) :
    ('a, 'v, 'b2, 'm) exp =
  let ul = unfold_lets ~context in
  match exp with
  | Bound (b, _) -> Hashtbl.find context b
  | Let (b, bs, e, _) ->
      List.iter
        (fun (b, e) ->
          let e = ul e in
          Hashtbl.add context b e)
        (b :: bs);
      let res = ul e in
      List.iter (Pair.iter (Hashtbl.remove context) ignore) (b :: bs);
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
(** {1 Memory operation }

    This section allow to go from expression without memory operations
    to expression with them and vice-versa.*)

(** Allow memory operations in an expression. *)
let allow_mem : ('a, 'v, 'b, no) exp -> ('a, 'v, 'b, 'm) exp =
  (* Check that exp is covariant in the 'm parameter, otherwise this is unsound *)
  let module M : sig
    type +'m t [@@ocaml.warning "-34"] (* unused type *)
  end = struct
    type 'm t = (no, no, no, 'm) exp
  end in
  Obj.magic

(** Same as {!allow_mem} but for the {!smt} type *)
let smt_allow_mem : ('a, 'v, 'b, no) smt -> ('a, 'v, 'b, 'm) smt =
  (* Check that smt is covariant in the 'm parameter, otherwise this is unsound *)
  let module M : sig
    type +'m t [@@ocaml.warning "-34"] (* unused type *)
  end = struct
    type 'm t = (no, no, no, 'm) smt
  end in
  Obj.magic

(** Same as {!allow_mem} but for the {!ty} type *)
let ty_allow_mem : no ty -> 'm ty =
  (* Check that ty is covariant in the 'm parameter, otherwise this is unsound *)
  let module M : sig
    type +'m t [@@ocaml.warning "-34"] (* unused type *)
  end = struct
    type 'm t = 'm ty
  end in
  Obj.magic

(** Check that not memory operation take place in that expression. Return [true]
    if that's the case and [false] otherwise.


    This is not resilient to change of type: If a new memory constructor is added, then
    this function will be wrong
*)
let check_no_mem (e : ('a, 'v, 'b, 'm) exp) : bool =
  let ref_res = ref true in
  let rec check = function
    | Binop (Binmem _, _, _, _) -> ref_res := false
    | e -> direct_exp_iter_exp check e
  in
  check e;
  !ref_res

(** Expect that an [exp] has no memory constructor, and then return it with
    memory removed from the type. Throws [Failure] if the value had memory constructors.

    This is not resilient to change of type, If a new memory constructor is added, then
    this function will be unsound.

    TODO: Find a way to make it resilient *)
let expect_no_mem ?(handler = fun () -> failwith "Expected no mem") :
    ('a, 'v, 'b, 'm1) exp -> ('a, 'v, 'b, 'm2) exp =
 fun exp -> if check_no_mem exp then Obj.magic exp else handler ()
