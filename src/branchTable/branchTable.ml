(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module is designed to symbolically evaluate all the branch tables in an ELF file. 
    Known bugs:
    - How to handle the following for live = [x9] ?
      40812c:       2a0803e9        mov     w9, w8
      408130:       2a0903e8        mov     w8, w9
      408134:       71000d08        subs    w8, w8, #0x3
    - Multiple starts *)

open Cmdliner
open Config.CommonOpt

open Logs.Logger (struct
  let str = __MODULE__
end)

module Analyses = InstrsAnalyses
module Reg = State.Reg
module Instr = Trace.Instr
module Runner = Run.Runner
module Block = Run.Block_lib
module ConcreteEval = Exp.ConcreteEval
module Value = Exp.Value

let pp_regs regs = Pp.(top (list Reg.pp) regs)

let pp_ptrs ptrs = Pp.(top (list ptr) ptrs)

type and_t = AndS of Reg.t * BitVec.t | Mask of Reg.t * Reg.t * BitVec.t

let looks_like_ands (trace0 : Instr.trace_meta) (trace1 : Instr.trace_meta) =
  (* Trace 1: [ Assert !(reg:R20[0:0] = 0:1), ... ] *)
  let assert_not_low_bit = function
    | Trace.Assert
        (Unop
          (Not, Binop (Eq, Unop (Extract (0, 0), Var (Register rn, _), _), Bits (bits, _), _), _))
      when BitVec.size bits = 1 ->
        Some (rn, bits)
    | _ -> None
  in
  (* Trace 0: [ Assert reg:R20[0:0] = 0:1, ... ] *)
  let assert_low_bit = function
    | Trace.Assert (Binop (Eq, Unop (Extract (0, 0), Var (Register rn, _), _), Bits (bits, _), _))
      when BitVec.size bits = 1 ->
        Some (rn, bits)
    | _ -> None
  in
  Option.(
    let* event0 = List.nth_opt trace0.trace 0 and* event1 = List.nth_opt trace1.trace 0 in
    let* (reg0, bit0) = assert_low_bit event0 and* (reg1, bit1) = assert_not_low_bit event1 in
    assert (Reg.(reg0 = reg1) && BitVec.(size bit0 = size bit1 && to_z bit0 = to_z bit1));
    Some (AndS (reg0, BitVec.zero_extend 31 bit0)))

let looks_like_and_low_mask rd value =
  match value with
  (* Beware of following, hence [hi < 31]
       Write |reg:R8| with 0x0:60@reg:R1[0:3]    -- "and w8, w1, #0xf"
       Write |reg:R2| with 0x0:32@reg:R19[0:31]  -- "mov w2, w19" *)
  | Ast.Manyop
      ( Concat,
        [Bits (maybeZeros, _); Unop (Extract (hi, 0), Var (Trace.Var.Register rn, _), _)],
        _ )
    when BitVec.to_z maybeZeros = Z.zero && hi < 31 ->
      Some (Mask (rd, rn, BitVec.of_int ~size:32 ((1 lsl (hi + 1)) - 1)))
  | _ -> None

let looks_like_and = function
  | [] | _ :: _ :: _ :: _ -> None
  | [trace0; trace1] -> looks_like_ands trace0 trace1
  | [{ Instr.trace; jump_target = _; read = _; written = _ }] -> (
      match trace with
      | [] | [(Trace.Assert _ | ReadMem _ | WriteMem _)] | _ :: _ :: _ -> None
      | [WriteReg { reg = rd; value }] -> looks_like_and_low_mask rd value
    )

let looks_like_csel = function
  | [] | _ :: _ :: _ -> false
  | [{ Instr.trace; jump_target = _; read = _; written = _ }] -> (
      match trace with
      | [] | [(Trace.Assert _ | ReadMem _ | WriteMem _)] | _ :: _ :: _ -> false
      | [WriteReg { reg = _; value }] -> (
          let is_reg = function Ast.Var (Trace.Var.Register _, _) -> true | _ -> false in
          let is_bool = function Ast.Bits (bits, _) -> BitVec.size bits = 1 | _ -> false in
          match value with
          (* Write |reg:R3| with if reg:PSTATE.Z = 1:1 then reg:R3 else reg:R0 *)
          | Ite (Binop (Eq, e1, bits, _), e2, e3, _)
            when is_reg e1 && is_bool bits && is_reg e2 && is_reg e3 ->
              true
          | _ -> false
        )
    )

type cmp_case = RangeFrom0 | Eq_Neq

(** Two things are 'fixed' for the reads and writes:
    - Reads of the PC and SP are removed since they are always live
    - Comparisons of a register with a constant are considered a write, because
      we set the value of the register with a range of constants before simulating. *)
let fixup_written_read instr_at addr { Instr.read; written; opcode; traces; length } =
  let always_live =
    let pc = Arch.pc () and sp = Arch.sp () in
    fun x -> x = pc || x = sp
  in
  let is_cmp_we_care_about =
    (* NOTE This is definitely wrong, but not worthwhile to do accurately.
       Specifically, the code assumes that
       - a conditional branch instruction is checking (the NZCV flags) to see
         if the register was was less than a bound
       - anything else checks merely for equality. *)
    Option.(
      let* res = Arch.is_cmp opcode in
      let rec loop limit addr length =
        if limit = 0 then (
          debug "No csel/b._ found after cmp";
          None
        )
        else
          (* Sadly, the branch/conditional instruction may not follow the cmp immediately *)
          let loop = loop (limit - 1) in
          let next_addr = addr + length in
          let* next = instr_at next_addr in
          match next with
          | Runner.Nocode -> None
          | Special next_len | IslaFail next_len -> loop next_addr next_len
          | Normal ({ length = next_len; traces; _ } as next) -> (
              match Analyses.looks_like_a_branch next next_addr with
              (* If a compare is followed by a conditional branch,
                 assume it needs a range of values. *)
              | Some (Analyses.Simple (_, falls_thru)) ->
                  if falls_thru then Some (RangeFrom0, res) else loop next_addr next_len
              | None ->
                  if looks_like_csel traces then Some (Eq_Neq, res) else loop next_addr next_len
              | Some (Reg _) -> None
            )
      in
      loop 5 addr length)
  in
  match is_cmp_we_care_about with
  | None -> (
      match looks_like_and traces with
      | None -> (is_cmp_we_care_about, written, List.filter Fun.(negate always_live) read)
      | Some (AndS (reg, imm)) ->
          ( Some (Eq_Neq, (reg, imm)),
            reg :: written,
            List.filter (fun x -> x <> reg && not (always_live x)) read )
      | Some (Mask (_rd, rn, imm)) ->
          ( Some (RangeFrom0, (rn, imm)),
            (* _rd is already here *) written,
            List.filter (fun x -> x <> rn && not (always_live x)) read )
    )
  | Some (_, (reg, _)) ->
      ( is_cmp_we_care_about,
        reg :: written,
        List.filter (fun x -> x <> reg && not (always_live x)) read )

type dataflow_context = {
  instr_at : int -> Runner.slot option;
  comes_before : int -> (int * int list) option;
  spills_for : int -> int list;
  depth_limit : int;
  relevant_instrs : (int, unit) Hashtbl.t;
  relevant_spills : (int, bool) Hashtbl.t;
  live : Reg.t list;
  cmp_operands : (cmp_case * (Reg.t * BitVec.t)) option;
}

exception DeadEnd

exception MultipleStarts

exception IrrelevantEarlyRange of cmp_case

(** A standard backwards-dataflow analysis with following modifications:
    - comparisons with constants are treated as writes
    - irrelevant instructions (those which do not write to any live registers) are ignored
    - relevant instructions are tracked and returned
    - the operands of the first relevant comparison with a constant (which is followed by
      a conditional branch) are also returned, if it exists *)
let rec update_live ctxt addr =
  debug "  visiting %#x" addr;
  if ctxt.depth_limit = 0 then fail "Hit branch table calculation depth limit"
  else
    let (is_cmp_we_care_about, written, read) =
      match ctxt.instr_at addr with
      | None | Some Runner.Nocode -> Raise.unreachable ()
      | Some (Special _ | IslaFail _) -> (None, [], [])
      | Some (Normal instr) -> fixup_written_read ctxt.instr_at addr instr
    in
    let (cmp_operands, live) =
      let (relevant_regs, rest) =
        List.partition (fun x -> List.mem Reg.( = ) x written) ctxt.live
      in
      let relevant_spill = Hashtbl.mem ctxt.relevant_spills addr in
      if relevant_spill then Hashtbl.replace ctxt.relevant_spills addr true;
      if not (relevant_regs <> [] || relevant_spill) then (
        ( match is_cmp_we_care_about with
        | Some (cmp_case, _) ->
            if ctxt.cmp_operands = None then raise (IrrelevantEarlyRange cmp_case)
        | None -> ()
        );
        (ctxt.cmp_operands, ctxt.live)
      )
      else (
        Hashtbl.add ctxt.relevant_instrs addr ();
        List.iter (Fun.flip (Hashtbl.add ctxt.relevant_spills) false) @@ ctxt.spills_for addr;
        let live = List.sort_uniq Reg.compare @@ read @ rest in
        debug "  instr at %#x writes %t, live %t" addr (pp_regs written) (pp_regs live);
        (* Save the operands of the first relevant compare. This is used later, for
           setting the value of the compared register with a range of values before
           simulating the branch-register calculation. *)
        (Option.fold ~none:is_cmp_we_care_about ~some:Option.some ctxt.cmp_operands, live)
      )
    in
    match live with
    | [] -> (
        let not_seen_spills =
          Hashtbl.fold
            (fun key seen acc -> if seen then acc else key :: acc)
            ctxt.relevant_spills []
        in
        match not_seen_spills with
        | [] -> (addr, ctxt.relevant_instrs, cmp_operands)
        | [addr] -> update_live { ctxt with live; cmp_operands } addr
        | _ :: _ -> fail "No idea how to implement this"
      )
    | _ :: _ -> step_backwards { ctxt with live; cmp_operands } addr

(* A standard backwards traversal with cycle-prevention and backtracking.
   If more than one answer, it fails - the simluation relies on straight-line execution. *)
and step_backwards ctxt addr =
  match ctxt.comes_before addr with
  | None -> raise DeadEnd
  | Some (next, nexts) -> (
      (* We use addresses to break cycles in the comes-before graph *)
      let filtered = List.filter (fun nxt -> nxt < addr) (next :: nexts) in
      match filtered with
      | [] ->
          info "  DeadEnd %t come before %#x" (pp_ptrs @@ (next :: nexts)) addr;
          raise DeadEnd
      | [addr] -> update_live { ctxt with depth_limit = ctxt.depth_limit - 1 } addr
      | _ :: _ :: _ -> (
          let ans = ref [] in
          debug "  Trying multiple paths %t" (pp_ptrs filtered);
          List.iter
            (fun addr ->
              try
                let ctxt =
                  {
                    ctxt with
                    depth_limit = ctxt.depth_limit - 1;
                    relevant_instrs = Hashtbl.copy ctxt.relevant_instrs;
                    relevant_spills = Hashtbl.copy ctxt.relevant_spills;
                  }
                in
                ans := update_live ctxt addr :: !ans
              with DeadEnd -> ())
            filtered;
          match !ans with
          | [ans] -> ans
          | [] -> raise DeadEnd
          | _ :: _ :: _ ->
              info "  More than one start found: %t"
                (pp_ptrs @@ List.map (fun (x, _, _) -> x) !ans);
              raise MultipleStarts
        )
    )

(** Work backwards through the dataflow of the registers to find the earliest
    instruction from which point to run a straight-line simluation.

    This function returns a tuple containing:
    - the address of the start of the branch-register target computation
    - the set of instruction addresses which are relevant between the start
      (inclusive) and the branch-register instruction (exclusive)
    - the register that must be set with a value before simulating the
      relevant instructions
    - the inclusive maximum value which that register should be set to *)
let reverse_dataflow instructions comes_before spills (reg_branches : Analyses.reg_branches) =
  let process (br, regs) =
    debug "Computing dataflow for branch at %#x with initial live registers %t" br (pp_regs regs);
    let ctxt =
      {
        instr_at = Hashtbl.find_opt instructions;
        comes_before = Hashtbl.find_opt comes_before;
        spills_for = Hashtbl.find_all (Lazy.force spills);
        depth_limit = 1000;
        relevant_instrs = Hashtbl.create 10;
        relevant_spills = Hashtbl.create 10;
        live = regs;
        cmp_operands = None;
      }
    in
    let (start, relevant_instrs, cmp_reg_bits_opt) = step_backwards ctxt br in
    (br, (start, relevant_instrs, cmp_reg_bits_opt))
  in
  List.map process (reg_branches :> (int * Reg.t list) list)

let make_block runner (br, (start, relevant, cmp_reg_bits_opt)) =
  (* Add the br instruction itself: this means the target is always in the PC *)
  Hashtbl.add relevant br ();
  ( br,
    ( (* The result of Block.gen_endpred keeps track of PCs that have been seen before,
         so you can't reuse it for multiple executions of the same sequence of instructions. *)
      (fun () ->
        (* Argument [~max] is treated as exclusive, so [~max:(br + 1)] to get
           the final target in the PC itself *)
        let endpred = Block.gen_endpred ~min:start ~max:(br + 1) ~loop:1 ~brks:[] () in
        Block.make ~runner ~start ~endpred),
      relevant,
      cmp_reg_bits_opt ) )

type pc_exp_guess = Sym of Block.label State.Tree.t | Ints of Block.label State.Tree.t array

let symeval_range_for_trees elf start (block, relevant, cmp_reg_bits_opt) =
  let start_with reg bits =
    let st = State.copy ~elf start in
    State.set_reg st reg { ctyp = None; exp = Exp.Typed.bits bits };
    State.lock st;
    st
  in
  match cmp_reg_bits_opt with
  | None -> Sym (Block.run ~relevant (block ()) start)
  | Some (Eq_Neq, (cmp_reg, cmp_bits)) ->
      let start_eq = start_with cmp_reg cmp_bits in
      let start_neq = start_with cmp_reg BitVec.(add (of_int ~size:64 1) cmp_bits) in
      Ints [|Block.run ~relevant (block ()) start_eq; Block.run ~relevant (block ()) start_neq|]
  | Some (RangeFrom0, (cmp_reg, cmp_bits)) ->
      let incl_max = BitVec.to_int cmp_bits in
      debug "Setting reg %t with values 0x0 - %#x" (Pp.top Reg.pp cmp_reg) incl_max;
      Ints
        (Array.init (incl_max + 1) (fun i ->
             Block.run ~every_instruction:true ~relevant (block ())
               (start_with cmp_reg (BitVec.of_int ~size:64 i))))

type 'a target = Sym of State.exp * (State.var * State.exp) list | Conc of 'a list

let map_target f = function Sym _ as x -> x | Conc xs -> Conc (List.map f xs)

let exp_to_int ?ctxt exp = exp |> ConcreteEval.eval ?ctxt |> Value.expect_bv |> BitVec.to_int

let target_to_ints =
  let zero = Value.Bv (BitVec.of_int ~size:64 0) in
  function Conc x -> x | Sym (x, _) -> [exp_to_int ~ctxt:(Fun.const zero) x]

let int_or_sym (st, exp) =
  try Conc [exp_to_int exp]
  with ConcreteEval.Symbolic ->
    debug "symbolic pc: %t" (Pp.top State.pp st);
    let to_addr_exp (block : State.Mem.Fragment.Block.t) =
      let open Exp.Typed in
      let offset = bits_int ~size:64 block.offset in
      Option.fold ~none:offset ~some:(Fun.flip ( + ) offset) block.base
    in
    let get_read : State.Mem.Fragment.Event.t -> _ option = function
      | Read (block, addr) -> Some (addr, to_addr_exp block)
      | Write _ -> None
    in
    Sym (exp, List.filter_map get_read @@ State.Mem.(Fragment.get_trace @@ get_main st.mem))

let pp_target =
  let open Pp in
  function
  | Conc x -> concat @@ List.map (fun x -> string ", " ^^ ptr x) x
  | Sym (x, reads) ->
      let open State in
      let pp_read (var, exp) = concat [bar; Var.pp var; bar; string " from "; Exp.pp exp] in
      concat [Exp.pp x; string " where "; list pp_read reads]

let extract_branch_targets pc : pc_exp_guess -> int target =
  let rec linear (x : _ State.Tree.t) = List.length x.rest <= 1 && List.for_all linear x.rest in
  let last_state tree =
    assert (linear tree);
    tree |> State.Tree.map_to_list (fun _ state -> state) |> List.last
  in
  let pc_exp state = State.(Tval.exp @@ get_reg state pc) in
  function
  | Sym tree -> last_state tree |> Pair.split |> Pair.map Fun.id pc_exp |> int_or_sym
  | Ints tree_array ->
      assert (Array.for_all linear tree_array);
      Conc
        (tree_array
        |> Array.map (fun tree -> last_state tree |> pc_exp |> exp_to_int)
        |> Array.to_list |> List.sort_uniq Int.compare
        )

let process_sym names (dwarf : Dw.t) start =
  let elf = dwarf.elf in
  Elf.SymTable.fold elf.symbols [] (fun sym acc ->
      if not (sym.typ = FUNC && (names = [] || List.mem String.equal sym.name names)) then acc
      else (
        info "Starting %s" sym.name;
        (* New runner each time to not accumulate instructions *)
        let runner = Runner.of_dwarf dwarf in
        let runner = { runner with dwarf = None } in
        Runner.load_sym runner sym;
        let (simp, reg) = Analyses.find_branches runner.instrs in
        try
          (simp, reg) |> Pair.map Pair.split Fun.id
          |> Pair.map
               Analyses.(
                 Pair.map (compute_comes_before runner.instrs) (fun x ->
                     lazy (track_spills runner.instrs x)))
               Fun.id
          |> Fun.uncurry (Fun.uncurry (reverse_dataflow runner.instrs))
          |> List.map (make_block runner)
          |> List.map (Pair.map Fun.id (symeval_range_for_trees elf start))
          |> List.map (Pair.map Fun.id (extract_branch_targets @@ Arch.pc ()))
          |> List.map (fun (addr, target) -> (addr, sym.name, target))
          |> Fun.flip ( @ ) acc
        with exn ->
          let brs = List.map fst (reg :> (int * Reg.t list) list) in
          ( match exn with
          | MultipleStarts ->
              warn "Not implemented: MultipleStarts for %t in %s" (pp_ptrs brs) sym.name
          | _ -> warn "Error processing %s, skipping branches %t" sym.name (pp_ptrs brs)
          );
          if names <> [] && List.mem String.equal sym.name names then raise exn
          else List.map (fun addr -> (addr, sym.name, Conc [])) brs @ acc
      ))

let get_branches log names elfname =
  (* Normally, handled by CommonOpt, but needs to be added for use by Analyse* *)
  (* Config.File.ensure_loaded Config.Default.config_file; *)
  if log then base "Finding branch tables used in %s" elfname;
  let dwarf = Dw.of_file elfname in
  let elf = dwarf.elf in
  if log then base "Loaded %s" elfname;
  Trace.Cache.start @@ Arch.get_isla_config ();
  let start =
    Run.Init.init () |> State.copy ~elf |> (Arch.get_abi { args = []; ret = None }).init
  in
  debug "Entry state:\n%t" (Pp.topi State.pp start);
  let answer = process_sym names dwarf start in
  Isla.Cache.stop ();
  answer

let symeval_branch_table elfname names =
  List.iter
    (fun (br, name, target) -> base "%#x in %s can jump to %t" br name (Pp.top pp_target target))
    (get_branches true names elfname)

let elf =
  let doc = "ELF file from which to pull the code" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"ELF_FILE" ~doc)

let funcs =
  let doc = "Restrict search to this function. Can be given multiple times." in
  Arg.(value & opt_all string [] & info ["func"] ~docv:"FUNCTION" ~doc)

let term = Term.(CmdlinerHelper.func_options comopts symeval_branch_table $ elf $ funcs)

let info =
  let doc =
    "Symbolically run the required instructions up to and including a branch-register \
     instruction."
  in
  Cmd.(info "branch-tables" ~doc ~exits)

let command = (term, info)
