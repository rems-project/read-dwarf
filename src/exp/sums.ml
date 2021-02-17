(* The documentation is in the mli file *)

let rec split =
  let open Ast in
  function
  | Manyop (Bvmanyarith Bvadd, l, _) -> List.concat_map split l
  | Unop (Extract (last, first), e, _) ->
      let l = split e in
      List.map (Typed.extract ~first ~last) l
  | Unop (Bvneg, e, _) ->
      let l = split e in
      List.map Typed.neg l
  | Binop (Bvarith Bvsub, e, e', _) ->
      let l = split e in
      let l' = split e' in
      let rl' = List.rev_map Typed.neg l' in
      List.rev_append rl' l
  | e -> [e]

let merge ~size l = if l = [] then Typed.zero ~size else Typed.sum l

let add_term ~term exp = term :: split exp |> Typed.sum

let remove_term ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> None
  | Some [] ->
      let size = Typed.expect_bv (Typed.get_type term) in
      Some (Typed.zero ~size)
  | Some l -> Some (Typed.sum l)

(** Same as {!remove_term} but if the term is not found, add the opposite to the sum*)
let smart_substract ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> Typed.sum @@ (Typed.neg term :: terms)
  | Some [] ->
      let size = Typed.expect_bv (Typed.get_type term) in
      Typed.zero ~size
  | Some l -> Typed.sum l

(** Split away the concrete terms of the sum *)
let split_concrete exp =
  let size = Typed.expect_bv (Typed.get_type exp) in
  let terms = split exp in
  let (symterms, concvals) = List.partition_map ConcreteEval.eval_if_concrete terms in
  let concbvs = List.map Value.expect_bv concvals in
  let concbv = List.fold_left BitVec.( + ) (BitVec.zero ~size) concbvs in
  let symterm = match symterms with [] -> None | _ -> Some (Typed.sum symterms) in
  (symterm, concbv)

(** Tells if an expression has a concrete term *)
let has_concrete_term exp = exp |> split |> List.exists ConcreteEval.is_concrete
