open Logs.Logger (struct
  let str = __MODULE__
end)

open Cmdliner
open Config.CommonOpt

module Sums = Exp.Sums
module Typed = Exp.Typed

module Var = struct
  type t = Left of State.var | Right of State.var

  let equal a b = match (a,b) with
  | Left a, Left b -> State.Var.equal a b
  | Right a, Right b -> State.Var.equal a b
  | _ -> false

  let pp = function
  | Left v -> Pp.(!^"L:" ^^ State.Var.pp v)
  | Right v -> Pp.(!^"R:" ^^ State.Var.pp v)

  let ty = function Left v | Right v -> State.Var.ty v

  let hash = Hashtbl.hash

  let of_string s =
    let v = State.Var.of_string @@ String.sub s 2 (String.length s - 2) in
    match String.sub s 0 2 with
    | "L:" -> Left v
    | "R:" -> Right v
    | _ -> Raise.inv_arg "Invalid variable: %s" s
end

module Exp = struct
  include Exp.Make (Var)

  let left : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Left v)

  let right : State.Exp.t -> t = Ast.Manip.exp_map_var (fun v -> Var.Right v)
end

module Z3sim = Z3.Make (Var)

type sem_type =
| Value of int
| Ptr of sem_type

type value_relation =
| Eq
| EqSection of string
| EqPage of string
| Indirect of sem_type

let rec pp_sem_type = Pp.(function
| Value w -> !^"Val"^^(int w)
| Ptr typ -> (pp_sem_type typ)^^(!^"*")
)

let pp_rel = Pp.(function
| Eq -> !^"Eq"
| EqSection s -> !^"EqSection " ^^ !^s
| EqPage s -> !^"EqPage " ^^ !^s
| Indirect typ -> !^"Indirect " ^^ (pp_sem_type typ)
)

let rec sem_type_of_type (typ: Ctype.t) : sem_type =
  match typ.unqualified with
  | Ctype.Machine _ | Ctype.Cint _ | Ctype.Cbool | Ctype.Enum _ -> Value (Ctype.sizeof typ)
  | Ptr { fragment=Ctype.DynArray typ'; _ } -> Ptr (sem_type_of_type typ')
  | _ -> Raise.todo()

let value_rel_for_type: Ctype.unqualified -> value_relation = function
| Ctype.Machine _ | Ctype.Cint _ | Ctype.Cbool | Ctype.Enum _ -> Eq
| Ptr { fragment=Ctype.Global s; _ } -> EqSection s
| Ptr { fragment=Ctype.DynFragment i; _ } -> EqSection ("Dyn_"^string_of_int i)
| Ptr { fragment=Ctype.DynArray typ'; _ } -> Indirect (sem_type_of_type typ')
| _ -> Raise.todo()

exception SimulationFailure of string

let fail_sim fmt =
  let fail msg = raise(SimulationFailure msg) in
  Printf.ksprintf fail fmt

let pp_diff pre pp l r =
  let open Pp in
  surround 2 2
    pre
    (!^"L: "^^pp l ^^ space ^^ !^"R: "^^pp r)
    empty

module ExpRel = struct
  type t = State.exp * value_relation * State.exp

  let to_exp ((exp1, rel, exp2):t) =
    let open Option in
    let modify e =
      match rel with
      | Eq -> e |> some
      | EqSection s -> Typed.(e - State.Exp.of_var (State.Var.Section s)) |> some
      (* TODO this is probably wrong: *)
      | EqPage s -> Typed.(e - concat [extract ~first:12 ~last:63 (State.Exp.of_var (State.Var.Section s)); bits_int ~size:12 0]) |> some
      | Indirect _ -> None
    in
    let+ e1, e2 = lift_pair (modify exp1, modify exp2) in
    Typed.((Exp.left e1) = (Exp.right e2))
 
  let pp ((a, r, b):t) =
    let open Pp in
    pp_diff
      (pp_rel r ^^ !^" between")
      Exp.pp (Exp.left a) (Exp.right b)
end

module RegRel = struct
  type t = value_relation State.Reg.Map.t

  let special_regs = ["OSDLR_EL1"; "OSLSR_EL1"; "EDSCR"; "SCR_EL3"]

  let infer_from_types (s:State.t) =
    State.Reg.Map.mapi (fun reg (r:State.Tval.t) ->
      if List.exists ((=) (State.Reg.to_string reg)) special_regs then
        Some Eq
      else
        Option.map (fun (r:Ctype.t) ->
          value_rel_for_type r.unqualified
        ) r.ctyp
    ) s.regs

  let to_exp_rel (s1:State.t) (s2:State.t) reg_rel : ExpRel.t list =
    let bindings = State.Reg.Map.bindings reg_rel in
    List.filter_map (fun (reg, rel) ->
      Option.map (fun rel ->
        State.get_reg_exp s1 reg, rel, State.get_reg_exp s2 reg
      ) rel
    ) bindings
end

module StackRel = struct
  module RelMap = RngMap.Make (struct
    type t = value_relation * int
    let len (_, sz: t) = sz
  end)
  type t = RelMap.t

  type loc = { offset:int; size:int }

  module Event = State.Mem.Fragment.Event

  let loc_of_blocks (blk1:State.Mem.Fragment.Block.t) (blk2:State.Mem.Fragment.Block.t) =
    if Option.is_some blk1.base || Option.is_some blk2.base then
      Raise.todo();
    if blk1.offset != blk2.offset || blk1.size != blk2.size then
      fail_sim "blocks don't match (%d, %t bytes) (%d, %t bytes)"
        blk1.offset (Pp.tos Ast.Size.pp_bytes blk1.size)
        blk2.offset (Pp.tos Ast.Size.pp_bytes blk2.size);
    { offset=blk1.offset; size=Ast.Size.to_bytes blk1.size }

  let rel_at_loc stack loc =
    let open Option in
    let* ((rel, relsz), reloff) = RelMap.at_off_opt stack loc.offset in
    if loc.size != relsz || reloff != 0 then
      (warn "Size not matching"; None)
    else
      Some rel
  
  let clear_loc stack loc =
    RelMap.clear stack ~pos:loc.offset ~len:loc.size  
  
  let infer_from_types ~stack_frag (st1:State.t) =
    let frag = Vec.get st1.fenv.frags stack_frag in
    let stack = ref RelMap.empty in
    State.Fragment.iteri (fun off ctype ->
      stack := RelMap.add !stack off (value_rel_for_type ctype.unqualified, Ctype.len ctype)
    ) frag;
    !stack
end

module GlobalRel = struct
  type eq_pair = State.exp * State.exp * sem_type

  type t = eq_pair list

  let find ~hyps (rel:t) a1 a2 =
    let check_one a1 a2 (a1', a2', typ) =
      debug "%t %t %t %t" (Pp.top State.Exp.pp a1) (Pp.top State.Exp.pp a2) (Pp.top State.Exp.pp a1') (Pp.top State.Exp.pp a2');
      let equal = Z3sim.check_full ~hyps Typed.(manyop Ast.And [Exp.left a1= Exp.left a1'; Exp.right a2= Exp.right a2']) in
      match equal with
      | Some true -> Some typ
      | _ -> None
    in
    List.find_map (check_one a1 a2) rel
  
  let add (rel:t) ((a1, a2, typ): eq_pair) =
    (a1, a2, typ)::rel

  let check ~hyps (rel:t) ((a1, a2, typ): eq_pair) =
    Option.map ((=) typ) (find ~hyps rel a1 a2)
  
  let rel_of_sem_type = function
  | Ptr t -> Indirect t
  | Value _ -> Eq
end

let block_addr (blk : State.Mem.Fragment.Block.t) =
  let ext = 64-Arch.address_size in
  match blk.base with
  | None -> Typed.bits_int ~size:64 blk.offset
  | Some b -> Typed.(unop (Ast.ZeroExtend ext) b + bits_int ~size:64 blk.offset)

let ptr_safety_asserts typ v =
  let sz = match typ with
  | Value x -> x
  | Ptr _ -> 8 (*assume 64 bit pointers*)
  in
  let topbits = (64 - Arch.address_size) in
  let small_enough = Typed.(extract ~first:Arch.address_size ~last:63 v = zero ~size:topbits) in
  
  let last = sz - 1 in
  let aligned = Typed.(extract ~first:0 ~last v = zero ~size:sz) in
  
  [small_enough; aligned]

module Context = struct
  type t = {
    asserts: Exp.t list;
    stack: StackRel.t;
    global: GlobalRel.t;
  }

  module Event = State.Mem.Fragment.Event

  let add_expr_rel (ctxt:t) rel =
    match rel with
    | (v1, Indirect t, v2) ->
        let safety1 = ptr_safety_asserts t v1 |> List.map Exp.left in
        let safety2 = ptr_safety_asserts t v2 |> List.map Exp.right in
        let nullptrs = Typed.((Exp.left v1 = zero ~size:64) = (Exp.right v2 = zero ~size:64)) in
        { 
          asserts = safety1 @ safety2 @ nullptrs::ctxt.asserts;
          global = GlobalRel.add ctxt.global (v1, v2, t);
          stack = ctxt.stack;
        }
    | rel ->
        let exp = Option.value_fail (ExpRel.to_exp rel) "Failed to convert relation to expression" in
        { ctxt with asserts = exp::ctxt.asserts }
  
  let check_expr_rel (ctxt:t) rel =
    match rel with
    | (v1, Indirect t, v2) ->
        GlobalRel.check ~hyps:ctxt.asserts ctxt.global (v1, v2, t)
    | rel ->
        let exp = Option.value_fail (ExpRel.to_exp rel) "Failed to convert relation to expression" in
        Z3sim.check_full ~hyps:ctxt.asserts exp

  let process_stack_operation event1 event2 (ctxt: t) =
    match event1, event2 with
    | Event.Read (blk1, v1), Event.Read (blk2, v2) ->
        let loc = StackRel.loc_of_blocks blk1 blk2 in
        let rel = StackRel.rel_at_loc ctxt.stack loc in
        ( match rel with
        | Some rel -> add_expr_rel ctxt (State.Exp.of_var v1, rel, State.Exp.of_var v2)
        | None -> (debug "No relation for stack read %t %t" (Pp.top Event.pp event1) (Pp.top Event.pp event2); ctxt)
        )
    | Event.Write (blk1, _exp1), Event.Write (blk2, _exp2) ->
        let loc = StackRel.loc_of_blocks blk1 blk2 in
        { ctxt with stack = StackRel.clear_loc ctxt.stack loc}
    | _ -> fail_sim "traces don't match %t %t" (Pp.tos Event.pp event1) (Pp.tos Event.pp event2)
  
  let process_global_operation event1 event2 (ctxt: t) =
    match event1, event2 with
    | Event.Read (blk1, v1), Event.Read (blk2, v2) ->
        let addr1 = block_addr blk1 in
        let addr2 = block_addr blk2 in
        let typ = GlobalRel.find ~hyps:ctxt.asserts ctxt.global addr1 addr2 in
        ( match typ with
        | Some typ ->
            let rel = GlobalRel.rel_of_sem_type typ in
            add_expr_rel ctxt (State.Exp.of_var v1, rel, State.Exp.of_var v2)
        | None -> (warn "No relation for global read %t %t" (Pp.top Event.pp event1) (Pp.top Event.pp event2); ctxt)
        )
    | Event.Write (blk1, exp1), Event.Write (blk2, exp2) ->
        let addr1 = block_addr blk1 in
        let addr2 = block_addr blk2 in
        let typ = GlobalRel.find ~hyps:ctxt.asserts ctxt.global addr1 addr2 in
        ( match typ with
        | Some typ ->
            let rel = GlobalRel.rel_of_sem_type typ in
            if check_expr_rel ctxt (exp1, rel, exp2) <> Some true then
              fail_sim "Unable to verify %t" (Pp.tos ExpRel.pp (exp1, rel, exp2))
        | None -> fail_sim "Unable to determine target type for global write %t %t" (Pp.tos Event.pp event1) (Pp.tos Event.pp event2)
        );
        ctxt
    | _ -> fail_sim "traces don't match %t %t" (Pp.tos Event.pp event1) (Pp.tos Event.pp event2)

  let infer_from_types ~stack_frag ~(dwarf:Dw.t) (state: State.t) =
    let stack = StackRel.infer_from_types state ~stack_frag in

    let regs = RegRel.infer_from_types state in
    let register_rels = RegRel.to_exp_rel state state regs in

    let global_variable_rels =
      Hashtbl.to_seq_values dwarf.vars
      |> Seq.map (fun (v:Dw.Var.t) ->
        let typ = sem_type_of_type v.ctype in
        match v.locs with
        | [_, Global addr] ->
            let addr = Elf.SymTable.to_addr_offset addr in
            let exp = State.Exp.of_address ~size:64 addr in
            (exp, Indirect typ, exp)
        | _ ->
            Raise.fail "Weird location description for global variable: %t" (Pp.tos Dw.Var.pp_raw v);
      )
      |> List.of_seq
    in

    let ctxt = { stack; asserts=[]; global=[] } in
    List.fold_left add_expr_rel ctxt (register_rels @ global_variable_rels)
end

type simrel = (State.Id.t*State.Id.t, Context.t) Hashtbl.t

let stack_prov = 0(* TODO determine stack_frag automatically *)
let stack_frag = 0(* TODO determine stack_frag automatically *)

exception SimulationFailureWithContext of {
  msg:string;
  states: State.t * State.t;
  ctxt: Context.t;
}

let rec checksim ~(rel:simrel) (s1:State.t) (s2:State.t) =
  Hashtbl.find_opt rel (s1.id, s2.id)
  |> Option.value_fun ~default:(fun() ->
    let bs1 = Option.value_fail s1.base_state "no base state" in
    let bs2 = Option.value_fail s2.base_state "no base state" in
    let prev_ctxt = checksim ~rel bs1 bs2 in
    try

      (* Process stack trace *)
      let ((_,mem1), (_,mem2)) = State.Mem.(get_frag s1.mem stack_prov, get_frag s2.mem stack_prov) in
      let (trc1, trc2) = State.Mem.Fragment.(get_trace mem1, get_trace mem2) in

      let ctxt = List.fold_right2 Context.process_stack_operation trc1 trc2 prev_ctxt in

      (* Process global trace *)
      let (mem1, mem2) = State.Mem.(get_main s1.mem, get_main s2.mem) in
      let (trc1, trc2) = State.Mem.Fragment.(get_trace mem1, get_trace mem2) in

      let ctxt = List.fold_right2 Context.process_global_operation trc1 trc2 ctxt in


      let asst1 = List.map Exp.left s1.asserts in
      let asst2 = List.map Exp.right s2.asserts in
      let impl1 = List.find_all Fun.(Z3sim.check_full ~hyps:(asst1@ctxt.asserts) %> flip Option.value_fail "TODO: Z3 failed" %> not) asst2 in
      let impl2 = List.find_all Fun.(Z3sim.check_full ~hyps:(asst2@ctxt.asserts) %> flip Option.value_fail "TODO: Z3 failed" %> not) asst1 in
      if not (List.is_empty impl1 && List.is_empty impl2) then
        fail_sim "%t" Pp.(Fun.const @@ sprint @@ pp_diff
          !^"States not equivalent on path conditions"
          (list Exp.pp)
          impl2
          impl1
      );

      Hashtbl.add rel (s1.id, s2.id) ctxt;
      ctxt
    with
    | SimulationFailure s -> raise @@ SimulationFailureWithContext {
        msg=s;
        states=(s1,s2);
        ctxt=prev_ctxt;
      }
  )

let check_return_values ~(ret_reg) ~(ret_type:Ctype.t) ~(rel:simrel) (s1:State.t) (s2:State.t) =
  let ctxt = Hashtbl.find rel (s1.id, s2.id) in

  let ret_val1 = State.get_reg_exp s1 ret_reg in
  let ret_val2 = State.get_reg_exp s2 ret_reg in
  let rel = value_rel_for_type ret_type.unqualified in

  if Context.check_expr_rel ctxt (ret_val1, rel, ret_val2) <> Some true then
    raise @@ SimulationFailureWithContext {
      msg=Printf.sprintf "Return values not equivalent
Condition: %t\n" (Pp.tos ExpRel.pp (ret_val1, rel, ret_val2));
      states=(s1,s2);
      ctxt=ctxt;
    }


let run elf name =
  let dwarf = Dw.of_file elf in
  let tree = Run.Func.get_state_tree ~elf ~name () in
  let initial_state = tree.state in
  
  let initial_ctxt = Context.infer_from_types ~stack_frag ~dwarf initial_state in

  let simrel:simrel = Hashtbl.create 10 in
  Hashtbl.add simrel (initial_state.id, initial_state.id) initial_ctxt;

  debug "%t" (Pp.top (State.Tree.pp_all Run.Block_lib.pp_label) tree);

  let ret = Option.(
    let* func =Dw.get_func_opt ~name dwarf in
    let+ typ = func.func.ret in
    if Ctype.sizeof typ > 8 then
      Raise.fail "unsupported return type %t" (Pp.tos Ctype.pp typ)
    else
      ((Arch.dwarf_reg_map()).(0), typ)
  ) in

  try
    State.Tree.prefix_iter (fun _ s ->
      checksim ~rel:simrel s s |> ignore;
      if State.get_reg_exp s (Arch.pc()) = State.Exp.of_var State.Var.RetAddr then
        Option.iter (fun (ret_reg, ret_type) ->
          check_return_values ~ret_reg ~ret_type ~rel:simrel s s;
        ) ret
    ) tree;
    base "Simulation successful"
  with
  | SimulationFailureWithContext e ->
      let st, _ = e.states in
      debug "Failing state: %t" (Pp.top State.pp st);
      base "Simulation failed:\n\n%s" e.msg

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let func =
  let doc = "Symbol name of the function to run" in
  Arg.(value & pos 1 string "main" & info [] ~docv:"FUNCTION" ~doc)

let term =
  Term.(
    CmdlinerHelper.func_options comopts run
    $ elf $ func)

let info =
  let doc =
    "Simulation relation on relocatable binary"
  in
  Cmd.(info "relsim" ~doc ~exits)

let command = (term, info)
