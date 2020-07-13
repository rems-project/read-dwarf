(** This module is about symbolic pointer manipulation

    All functions that expect expression in this module expect expresion of size
    {!pointer_size} except {!to_ptr_size}. Use {!to_ptr_size} to convert a machine
    pointer sized expression to {!pointer_size}
*)

let pointer_size = Arch.address_size

type 'v exp = (Isla.lrng, 'v, Ast.no, Ast.no) Ast.exp

type 'v t = { offset : int; vars : 'v list; terms : 'v exp list }

let is_const (ptr : 'v t) = ptr.vars = [] && ptr.terms = []

let is_base_offset (ptr : 'v t) =
  match ptr.vars with [_] when ptr.terms = [] -> true | _ -> false

let nullptr = { offset = 0; vars = []; terms = [] }

(** Convert a full size pointer (64 bits) to the {!pointer_size} *)
let to_ptr_size (exp : 'v exp) = Ast.Op.extract (pointer_size - 1) 0 exp

module Term = struct
  type 'v ptr = 'v t

  type 'v t = Const of int | Var of 'v | Exp of 'v exp

  let of_exp (exp : 'v exp) =
    let open Ast in
    match exp with
    | Bits (bv, _) -> Const (BitVec.to_int bv)
    | Unop (Extract (a, 0), Var (v, _), _) when a = pointer_size - 1 -> Var v
    | e -> Exp e
end

let add_term (ptr : 'v t) : 'v Term.t -> 'v t = function
  | Const c -> { ptr with offset = ptr.offset + c }
  | Var v -> { ptr with vars = v :: ptr.vars }
  | Exp e -> { ptr with terms = e :: ptr.terms }

let of_term : 'v Term.t -> 'v t = function
  | Const c -> { offset = c; vars = []; terms = [] }
  | Var v -> { offset = 0; vars = [v]; terms = [] }
  | Exp e -> { offset = 0; vars = []; terms = [e] }

(** Convert a full expression to the pointer format (only address_size bits wide)

    The expression must have been Z3 simplified for bests result (no bvsub, no nested bvadd, ...) *)
let of_exp (exp : 'v exp) : 'v t =
  let open Ast in
  match exp with
  | Manyop (Bvmanyarith Bvadd, el, _) ->
      el |> List.map Term.of_exp |> List.fold_left add_term nullptr
  | _ -> of_term (Term.of_exp exp)
