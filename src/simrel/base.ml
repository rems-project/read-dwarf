(** This module encodes the definition and checking of the simulation relation. *)

module Tree = State.Tree
module ExTy = Exp.Typed
module Tval = State.Tval
module Reg = State.Reg
module Simplify = State.Simplify
module Z3St = State.Simplify.Z3St
module Id = State.Id

type tree = Run.Block_lib.label Tree.t

type 'a pair = { o0 : 'a; o2 : 'a }

let rel_of related x y =
  let exps = List.map (fun { o0; o2 } -> ExTy.[Tval.exp x = o0; Tval.exp y = o2]) related in
  ExTy.(manyop Ast.Or @@ (ExTy.(Tval.exp x = Tval.exp y) :: List.map (manyop Ast.And) exps))

type field = Reg of Reg.t

type sep = Eq of field pair list

(** Same should always be at the end. *)
type rel = Same | Star of sep * rel

type matched = Matched of State.t pair * rel | NoMatch of State.t

type rels = matched list

module Test2 = struct
  let infer_rels state_tree : rels =
    let to_list = State.Tree.map_to_list (fun _ st -> st) in
    match List.combine (to_list state_tree.o0) (to_list state_tree.o2) with
    | [] | [_] | _ :: _ :: _ :: _ -> Raise.unreachable ()
    | [(st1, st2); (end1, end2)] ->
        let (r8, r9, r10) = Reg.(of_string "R8", of_string "R9", of_string "R10") in
        [
          Matched ({ o0 = st1; o2 = st2 }, Same);
          Matched
            ( { o0 = end1; o2 = end2 },
              Star (Eq [{ o0 = Reg r9; o2 = Reg r8 }; { o0 = Reg r10; o2 = Reg r9 }], Same) );
        ]
end

module Test3 = struct
  (* return p ? ( !p ? -2 : p->refcount++ ) : -1 *)
  (* In O0, there are 6 states, 3:(-1), 5:(p->refcount), 6:(-2) *)
  (* In O2, there are 4 states, 3:(-1), 4:(p->refcount)         *)

  let infer_rels state_tree : rels =
    let to_list = State.Tree.map_to_list (fun _ st -> st) in
    let[@ocaml.warning "-8"] [o0_1; _; o0_3; _; o0_5; o0_6] = to_list state_tree.o0 in
    let[@ocaml.warning "-8"] [o2_1; _; o2_3; o2_4] = to_list state_tree.o2 in
    [
      Matched ({ o0 = o0_1; o2 = o2_1 }, Same) (* first *);
      Matched ({ o0 = o0_3; o2 = o2_3 }, Same) (* -1 *);
      Matched ({ o0 = o0_5; o2 = o2_4 }, Same) (* p->refcount *);
      NoMatch o0_6 (* the impossible case *);
    ]
end

module MemRel = struct
  type block = State.Mem.Fragment.Block.t

  let of_int x =
    let size = 64 in
    ExTy.bits_int ~size x

  let eq_block (block1 : block) (block2 : block) =
    ( ExTy.(of_int block1.offset = of_int block2.offset),
      match (block1.base, block2.base) with
      | (Some exp1, Some exp2) -> ExTy.(exp1 = exp2)
      | _ -> Raise.todo () )

  type event = State.Mem.Fragment.Event.t

  type trace = event list

  (** For now, just assume an equality relation for memory *)
  let eq (trc1 : trace) (trc2 : trace) =
    let f acc (x : event) (y : event) =
      match (x, y) with
      | (Read (block1, var1), Read (block2, var2)) ->
          let (x, y) = eq_block block1 block2 in
          let (ty1, ty2) =
            Ast.(Ty_BitVec (Size.to_bits block1.size), Ty_BitVec (Size.to_bits block2.size))
          in
          ExTy.(var ~typ:ty1 var1 = var ~typ:ty2 var2) :: x :: y :: acc
      | (Write (block1, exp1), Write (block2, exp2)) ->
          let (x, y) = eq_block block1 block2 in
          ExTy.(exp1 = exp2) :: x :: y :: acc
      | (_, _) -> Raise.todo ()
    in
    List.fold_left2 f [] trc1 trc2

  let eq (end1 : State.t) (end2 : State.t) =
    let module Mem = State.Mem in
    let (mem1, mem2) = Mem.(get_main end1.mem, get_main end2.mem) in
    let (trc1, trc2) = Mem.Fragment.(get_trace mem1, get_trace mem2) in
    eq trc1 trc2
end

module RegRel = struct
  let reg_map_fold e f map =
    let ref = ref e in
    map |> Reg.Map.iteri (fun reg tval -> ref := f !ref reg tval);
    !ref

  let present except reg = match except with None -> false | Some x -> List.mem reg x

  (** This is not symmetric (does it need to be?). 
    NOTE: If there is a reg in st2 that is not in st1, it will not be checked.
    Example: test3, O2, state 1, __defaultRAM -> |reg:0:__defaultRAM| (not in O0) *)
  let eq ?except:dont_include (st1 : State.t) (st2 : State.t) =
    let list =
      reg_map_fold []
        (fun acc reg tval1 ->
          if present dont_include reg then acc
          else
            let tval2 = Reg.Map.get st2.regs reg in
            (* NOTE: Tval.t = { ctyp : Ctype.t option; exp : Exp.t },
               the below ignores ctyp *)
            let (exp1, exp2) = (Tval.exp tval1, Tval.exp tval2) in
            (* Conjecture: it is valid and useful (efficiency,
               easier-to-understand output) to eliminate comparing terms that are
               syntactically equal *)
            if State.Exp.(equal exp1 exp2) then acc
            else ExTy.(Tval.exp tval1 = Tval.exp tval2) :: acc)
        st1.regs
    in
    ExTy.manyop Ast.And list
end

let sep_to_exp st =
  let get st = function Reg reg -> State.get_reg st reg in
  let get st field = Tval.exp (get st field) in
  function
  | Eq fields ->
      let ignore = List.fold_left (fun acc { o0; o2 } -> o0 :: o2 :: acc) [] fields in
      let exps = List.map (fun field -> ExTy.(get st.o0 field.o0 = get st.o2 field.o2)) fields in
      (ignore, [ExTy.manyop Ast.And exps])

let assn_to_exp (st : State.t) =
  match st.asserts with [] -> ExTy.true_ | _ :: _ -> ExTy.manyop Ast.And st.asserts

let rel_to_exp st rel =
  let rec rel_to_exp st exclude = function
    | Same ->
        (* NOTE: ignoring stack-locals for now *)
        let except = List.map (function Reg reg -> reg) exclude in
        let mem = MemRel.eq st.o0 st.o2 in
        let assn = ExTy.(assn_to_exp st.o0 = assn_to_exp st.o2) in
        let regs = RegRel.eq ~except st.o0 st.o2 in
        regs :: assn :: mem
    | Star (sep, rel) ->
        let (excl, exp1) = sep_to_exp st sep in
        let exp2 = rel_to_exp st (excl @ exclude) rel in
        exp1 @ exp2
  in
  rel_to_exp st [] rel

let check_rel rel =
  let matched_or_not = function
    | Matched (st, rel) -> rel_to_exp st rel
    | NoMatch st -> [ExTy.(not @@ manyop Ast.And st.asserts)]
  in
  let whole = ExTy.manyop Ast.And (List.concat_map matched_or_not rel) in
  match Z3St.check_sat_full [whole] with None -> Raise.todo () | Some x -> x
