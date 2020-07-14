let rec split =
  let open Ast in
  function
  | Manyop (Bvmanyarith Bvadd, l, _) -> List.concat_map split l
  | Unop (Extract (last, first), e, _) ->
      let l = split e in
      List.map (ExpTyped.extract ~first ~last) l
  | Unop (Bvneg, e, _) ->
      let l = split e in
      List.map ExpTyped.neg l
  | Binop (Bvarith Bvsub, e, e', _) ->
      let l = split e in
      let l' = split e' in
      let rl' = List.rev_map ExpTyped.neg l' in
      List.rev_append rl' l
  | e -> [e]

let merge ~size l = if l = [] then ExpTyped.zero ~size else ExpTyped.sum l

let add_term ~term exp = term :: split exp |> ExpTyped.sum

let remove_term ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> None
  | Some [] ->
      let size = ExpTyped.expect_bv (ExpTyped.get_type term) in
      Some (ExpTyped.zero ~size)
  | Some l -> Some (ExpTyped.sum l)

(** Same as {!remove_term} but if the term is not found, add the opposite to the sum*)
let smart_substract ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> ExpTyped.sum @@ (ExpTyped.neg term :: terms)
  | Some [] ->
      let size = ExpTyped.expect_bv (ExpTyped.get_type term) in
      ExpTyped.zero ~size
  | Some l -> ExpTyped.sum l

(** Split away the concrete terms of the sum *)
let split_concrete exp =
  let size = ExpTyped.expect_bv (ExpTyped.get_type exp) in
  let terms = split exp in
  let (symterms, concvals) = List.partition_map ConcreteEval.eval_if_concrete terms in
  let concbvs = List.map Value.expect_bv concvals in
  let concbv = List.fold_left BitVec.( + ) (BitVec.zero ~size) concbvs in
  let symterm = match symterms with [] -> None | _ -> Some (ExpTyped.sum symterms) in
  (symterm, concbv)

(** Tells if an expression has a concrete term *)
let has_concrete_term exp = exp |> split |> List.exists ConcreteEval.is_concrete
