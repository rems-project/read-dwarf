(** This module encodes the definition and checking of the simulation relation. *)

module Tree = State.Tree
module ExTy = Exp.Typed
module Tval = State.Tval
module Reg = State.Reg
module Simplify = State.Simplify
module Z3St = State.Simplify.Z3St
module Id = State.Id

type tree = Run.Block_lib.label Tree.t

let reg_map_fold e f map =
  let ref = ref e in
  map |> Reg.Map.iteri (fun reg tval -> ref := f !ref reg tval);
  !ref

let present except reg =
  match except with
  | None -> false
  | Some x -> List.mem reg x

let all_regs_eq ?except:dont_include (st1 : State.t) (st2 : State.t) =
  let list =
    reg_map_fold []
      (fun acc reg tval1 ->
         if present dont_include reg then
           acc
         else
           let tval2 = Reg.Map.get st2.regs reg in
           (* NOTE: Tval.t = { ctyp : Ctype.t option; exp : Exp.t },
              the below ignores ctyp *)
           let (exp1, exp2) = Tval.exp tval1, Tval.exp tval2 in
           (* Conjecture: it is valid and useful (efficiency,
              easier-to-understand output) to eliminate comparing terms that are
              syntactically equal *)
           if State.Exp.(equal exp1 exp2) then
             acc
           else
              ExTy.(Tval.exp tval1 = Tval.exp tval2) :: acc)
      st1.regs
  in
  ExTy.manyop Ast.And list

type 'a pair = { o0 : 'a; o2 : 'a }

let match_up tree_pair =
  let to_list = State.Tree.map_to_list (fun _ st -> st) in
  List.combine (to_list tree_pair.o0) (to_list tree_pair.o2)

type sep =
 | EqVal of Reg.t * (Z3St.var, Ast.no) ExTy.t pair list
 | EqReg of Reg.t pair list

type rel = Same | Sep of sep | Star of rel * rel

let of_int x =
  let size = 64 in
  ExTy.bits_int ~size x

module Test2 = struct

  let pc_rel =
    let related = [ { o0 = 0x4115c4; o2 = 0x4093b8 } ] in
    fun x y ->
      let exps = List.map (fun {o0;o2} -> ExTy.([ Tval.exp x = of_int o0; Tval.exp y = of_int o2])) related in
      (* the first (x = y) is to accommodate the return address, which for now,
         seems to be syntactically equal (same variable name) and would be
         assumed to be related anyways *)
      ExTy.(manyop Ast.Or @@ ExTy.(Tval.exp x = Tval.exp y) :: List.map (manyop Ast.And) exps)
  
  (** We don't actually care about this (as the last state, these registers
      would be subject to the ABI constraints on a return) but the code is
      there just to show what it would look like if we did care *)
  let non_pc_rel st1 st2 =
    let r8, r9, r10 = Reg.(of_string "R8", of_string "R9", of_string "R10") in
    let ignore = [r8; r9; r10] in
    let related = [ { o0 = r9; o2 = r8 }; { o0 = r10; o2 = r9 } ] in
    let exps = List.map (fun {o0;o2} ->
      ExTy.(Tval.exp @@ State.get_reg st1 o0 = Tval.exp @@ State.get_reg st2 o2)) related in
    (ignore, ExTy.(manyop Ast.And exps))
  
  let reg_rel st1 st2 =
    let (ignore, non_pc_rel) = non_pc_rel st1 st2 in
    let pc = Arch.pc () in
    let pc_rel = pc_rel (State.get_reg st1 pc) (State.get_reg st2 pc) in
    (pc :: ignore, [non_pc_rel; pc_rel] )
  
  type block = State.Mem.Fragment.Block.t
  
  let eq_block (block1 : block) (block2 : block) =
    (ExTy.(of_int block1.offset = of_int block2.offset)
    , (match block1.base, block2.base with
           | Some exp1, Some exp2 -> ExTy.(exp1 = exp2)
           | _ -> Raise.todo ()))
  
  type mem_event = State.Mem.Fragment.Event.t
  type mem_trace = mem_event list
  
  (** For now, just assume an equality relation for memory *)
  let mem_rel (trc1 : mem_trace) (trc2 : mem_trace) =
    let f acc (x : mem_event) (y : mem_event) =
      match x, y with
      | Read (block1, var1), Read (block2, var2) ->
        let x, y = eq_block block1 block2 in
        let ty1, ty2 = Ast.(Ty_BitVec (Size.to_bits block1.size)
                           , Ty_BitVec (Size.to_bits block2.size)) in
        ExTy.(var ~typ:ty1 var1 = var ~typ:ty2 var2) :: x :: y :: acc
  
      | Write (block1, exp1) , Write (block2, exp2) ->
        let x, y = eq_block block1 block2 in
        ExTy.(exp1 = exp2) :: x :: y :: acc
  
      | _ , _ -> Raise.todo () in
    List.fold_left2 f [] trc1 trc2
  
  let mem_rel (end1 : State.t) (end2 : State.t) =
     let module Mem = State.Mem in
     let mem1, mem2 = Mem.(get_main end1.mem, get_main end2.mem) in
     let trc1, trc2 = Mem.Fragment.(get_trace mem1, get_trace mem2) in
     mem_rel trc1 trc2
  
  (** Gather all the relations: ignoring stack-locals for now because they're
      irrelevant before a return anyway (also a bit of a faff) *)
  let end_rel (end1 : State.t) (end2 : State.t) =
    let (except, reg_rel)  = reg_rel end1 end2 in
    let mem_rel = mem_rel end1 end2 in
    let rest_eq = all_regs_eq ~except end1 end2 in
    rest_eq :: reg_rel @ mem_rel
  
  (** test2 only has 2 states *)
  let related = function
    | [] -> true
    | [ _ ] | _ :: _ :: _ :: _ -> Raise.unreachable ()
    | [ (st1, st2) ; (end1, end2) ] -> (
        let first_same = all_regs_eq st1 st2 in
        let rest_related = end_rel end1 end2 in
        let whole = ExTy.manyop Ast.And (first_same :: rest_related) in
        match Z3St.check_sat_full [whole] with None -> Raise.todo () | Some x -> x
      )
  
  end
