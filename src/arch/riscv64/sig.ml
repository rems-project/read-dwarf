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

(** This module provides the {!Sig} interface for the RV64 architecture

    All the ABI related function are derived from
    {{:https://github.com/riscv/riscv-elf-psabi-doc[the RISC-V ELF psABI Document]}}
*)

(** Describe the C API of a function *)
type func_api = { args : Ctype.t list; ret : Ctype.t option }

(** Describe the ABI of a function

    This is a record because I expect to add many other fields later.
*)
type func_abi = {
  init : State.t -> State.t;
      (** Gives the initial state for verifying the function, from a given global
          register state. Only global registers are kept. *)
}

module Reg = State.Reg
module Fragment = State.Fragment

(** The map of dwarf register: Which register number map to which ISA register *)
type dwarf_reg_map = Reg.t array

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal register info } *)

let nop_int : Int32.t = 0x00000012l

(** All the internal data that should be loaded *)
type t = {
  config : Config.File.ArchConf.t;
  reg_map : dwarf_reg_map;
  local_regs : bool Reg.Map.t;
  nop : BytesSeq.t;
  pc : Reg.t;
  sp : Reg.t;
}

(** Is the module loaded *)
let data = ref None

(** Get the loaded data, fails if the module wasn't loaded with {!init} *)
let get_data () =
  match !data with
  | Some d -> d
  | None -> fatal "Someone tried to use RISC-V 64 module before loading it"

(** Generates the register map *)
let gen_reg_map () =
  let res = Array.init 32 (fun i -> Reg.ensure_add [Printf.sprintf "x%d" i] (Ast.Ty_BitVec 64)) in
  res

(** Generates the Register map of local registers *)
let gen_local (dwarfregs : dwarf_reg_map) : bool Reg.Map.t =
  (* The PC is local in a certain sense, I don't want to match it in any way *)
  let pc = Reg.ensure_add ["PC"] (Ast.Ty_BitVec 64) in
  let local = Reg.Map.init (fun _ -> false) in
  Array.iter (fun reg -> Reg.Map.set local reg true) dwarfregs;
  Reg.Map.set local pc true;
  local

(** Generate the nop instruction *)
let gen_nop () =
  let nop = Bytes.create 4 in
  Bytes.set_int32_le nop 0 nop_int;
  BytesSeq.of_bytes nop

let gen_pc () = Reg.of_string "PC"

let gen_sp () = Reg.of_string "x2"

(** Generate the internal arch data *)
let gen_t () =
  let config = Config.(File.get_arch_config Arch.RISCV64) in
  let reg_map = gen_reg_map () in
  let local_regs = gen_local reg_map in
  let nop = gen_nop () in
  let pc = gen_pc () in
  let sp = gen_sp () in
  { config; reg_map; local_regs; nop; pc; sp }

(** Initialize the module. Internal function *)
let actual_init () =
  info "Loading RISC-V 64 module";
  if !data = None then data := Some (gen_t ()) else warn "RISC-V 64 was loaded multiple times"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal ABI implementation } *)

(** The internal ABI representation *)
type abi_repr = {
  reg_num : int;  (** The number of register used as regular arguments *)
  reg_types : Ctype.t array;  (** The types of those register (length reg_type = reg_num *)
  stack_length : int;  (** The number of stack bytes used to store arguments *)
  stack_fragment : Fragment.t;  (** The fragment with their types *)
  ret_pointer : Ctype.t option;  (** The optional return pointer type *)
}

(** TODO move that elsewhere *)
let roundupto x align = (x + align - 1) / align * align

(** Get an {!abi_repr} from an API ({!func_api})

    Follows the psABI's Procedure Calling Convention chapter.
*)
let get_abi_repr api : abi_repr =
  let open Ctype in
  (* Replace array by pointer to Single of the array *)
  let cdecay arg = if is_array arg then of_frag (Single arg) else arg in
  let args = List.map cdecay api.args in

  let process_args args : abi_repr =
    let reg_num = ref 0 in
    let reg_types = Array.make 8 (machine 8) in
    let stack_length = ref 0 in
    let stack_fragment = ref Fragment.empty in
    let add_to_stack typ =
      stack_fragment := Fragment.add !stack_fragment !stack_length typ;
      stack_length := roundupto (!stack_length + sizeof typ) 8
    in
    let fits_in_xlen typ =
      if !reg_num < 8 then begin
          reg_types.(!reg_num) <- typ;
          incr reg_num
        end else
        add_to_stack typ
    in
    let process_arg arg =
      (* TODO: is the psABI notion of scalar the same? *)
      if is_scalar arg then
        if sizeof arg <= 8 then
          fits_in_xlen arg
        else
          (* TODO: maybe fail? *)
          debug "Argument type unsupported: %t" (Pp.topi Ctype.pp arg)
      else if is_composite arg then
        if sizeof arg <= 8 then
          fits_in_xlen arg
        else if sizeof arg <= 16 && !reg_num < 8 then
          (* Following aarch64, if >1 reg then don't provide type *)
          (fits_in_xlen (machine 8); fits_in_xlen (machine 8))
        else
          add_to_stack arg
      else
        (* TODO: maybe fail? *)
        debug "Argument type unsupported: %t" (Pp.topi Ctype.pp arg)
    in
    List.iter process_arg args;
    {
      reg_num = !reg_num;
      reg_types;
      stack_length = !stack_length;
      stack_fragment = !stack_fragment;
      ret_pointer = None;
    }
  in
  let irepr = process_args args in

  (* Return value handling, according to section 5.5*)
  let ret_pointer =
    match api.ret with
    | None -> None
    | Some ret -> if sizeof ret > 16 then Some (of_frag ~restrict:true (Single ret)) else None
  in
  { irepr with ret_pointer }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 SigAPI }

    The implementation of the {!Sig} API. Go there for documentation *)

module Type = Config.Arch

let supports = function Type.RISCV64 -> true | _ -> false

let init = function
  | Type.RISCV64 -> actual_init ()
  | a ->
      Raise.fail "This version was compiled with only riscv64 support. %t is not supported"
        Pp.(tos Type.pp a)

let initialized () = if !data <> None then Some Type.RISCV64 else None

let module_name = "riscv64"

let loaded_name = "riscv64"

let address_size = 52

let dwarf_reg_map () = (get_data ()).reg_map

let is_local (reg : Reg.t) =
  let data = get_data () in
  Reg.Map.get data.local_regs reg

let nop () = (get_data ()).nop

let pc () = (get_data ()).pc

let sp () = (get_data ()).sp

let get_abi api =
  let open Ctype in
  let data = get_data () in
  let repr = get_abi_repr api in
  let sp = data.reg_map.(2) in
  let ret_pointer_reg = data.reg_map.(10) in
  let ra = data.reg_map.(1) in
  let init (state : State.t) =
    let state = State.copy_if_locked state in
    debug "Map:\n%t" (Pp.topi (Reg.Map.pp Pp.bool) data.local_regs);
    Reg.Map.iteri
      (fun reg b ->
        if b then begin
          debug "register to be reset %t" (Pp.top Reg.pp reg);
          State.reset_reg state reg
        end)
      data.local_regs;
    for i = 0 to repr.reg_num - 1 do
      (* TODO: is (Arg i) correct for >64 bits args? *)
      let tval = State.Tval.of_var ~ctyp:repr.reg_types.(i) (Arg i) in
      State.set_reg state data.reg_map.(i+10) tval
    done;
    ( match repr.ret_pointer with
    | None -> ()
    | Some ctyp -> State.set_reg state ret_pointer_reg (State.Tval.of_var ~ctyp RetArg)
    );
    let stack_frag_id = Fragment.Env.add_frag ~frag:repr.stack_fragment state.fenv in
    let stack_provenance =
      State.Mem.new_frag state.mem
        (State.get_reg_exp state sp |> Exp.Typed.extract ~last:(address_size - 1) ~first:0)
    in
    State.set_reg_type state sp
      (Ctype.of_frag ~provenance:stack_provenance @@ DynFragment stack_frag_id);
    State.set_reg state ra
      (State.Tval.of_var ~ctyp:(Ctype.of_frag_somewhere (Ctype.Global ".text")) RetAddr);
    let sp_exp = State.Exp.of_reg state.id sp in
    (* Assert that Sp is 16 bytes aligned *)
    State.push_assert state Exp.Typed.(extract ~last:3 ~first:0 sp_exp = bits_int ~size:4 0);
    (* TODO: any restrictions on SP values? *)
    (* Assert that there is enough stack space *)
    State.push_assert state Exp.Typed.(comp Ast.Bvuge sp_exp (bits_int ~size:64 0x1000));
    State.lock state;
    state
  in
  { init }

let assemble_to_elf instr =
  let data = get_data () in
  let toolchain = data.config.toolchain in
  let assembler = toolchain ^ "-as" in
  let linker = toolchain ^ "-ld" in
  let num = Random.bits () in
  let obj_file = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "%d.o" num) in
  let elf_file = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "%d.elf" num) in
  let prefix = ".global instr\n.type instr, @function\n.size instr,4\ninstr:" in
  Cmd.call_send_string
    [|assembler; "-g"; "-o"; obj_file|]
    (prefix ^ instr ^ "\n");
  Cmd.call [|linker; obj_file; "--entry=instr"; "-o"; elf_file|];
  Sys.remove obj_file;
  elf_file

let split_into_instrs (data: Elf.Symbol.data) = 
  let module IMap = Elf.Relocations.IMap in
  let rawdata = BytesSeq.to_listbs ~len:4 data.data in
  List.mapi (fun pos bytes -> 
      let (_, rel, rest) = IMap.split pos data.relocations in
      if Option.is_some @@ IMap.find_first_opt (fun i -> i < pos + 4) rest then
        Raise.fail "Misaligned relocation";      
      Elf.Symbol.{
        data = bytes;
        relocations = rel |> Option.map (IMap.singleton 0) |> Option.value ~default:IMap.empty;
      }
    ) rawdata

let is_ret code =
  assert (BytesSeq.length code = 4);
  (BytesSeq.get32le code 0) = 0x00008067l
(*
  let (( = ), ( land )) = Int32.(( = ), logand) in
  let code = BytesSeq.get32be code 0 in
  (* 0xc0035fd6 *)
  let zero_out_reg_operand_mask = 0x01fcffffl in
  let ret_with_reg_zero = 0x00005fd6l in
  zero_out_reg_operand_mask land code = ret_with_reg_zero
*)

(*$
  ;;
  inject

  let to_be_bytes_seq int32 =
    let result = Bytes.create 4 in
    Bytes.set_int32_le result 0 int32;
    BytesSeq.of_bytes result
*)

(*$T is_ret
     (0xd65f0000l |> to_be_bytes_seq |> is_ret)
     (0xd65f01e0l |> to_be_bytes_seq |> is_ret)
     (0xd65f02c0l |> to_be_bytes_seq |> is_ret)
     (0xd65f03a0l |> to_be_bytes_seq |> is_ret)
     (0xd65f0490l |> to_be_bytes_seq |> is_ret |> not)
     (0xd65f0770l |> to_be_bytes_seq |> is_ret |> not)
     (0xd65f0b50l |> to_be_bytes_seq |> is_ret |> not)
     (0xd65f0f30l |> to_be_bytes_seq |> is_ret |> not)
     (0x9100001fl |> to_be_bytes_seq |> is_ret |> not)
*)

(*${*)
(*
(** https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions/CMP--immediate---Compare--immediate---an-alias-of-SUBS--immediate-- *)
let is_cmp' code_bs =
  assert (BytesSeq.length code_bs = 4);
  let (( = ), ( land )) = Int32.(( = ), logand) in
  let code = BytesSeq.get32be code_bs 0 in
  let zero_out_operands_mask = 0x1f00807fl in
  let cmp_with_zero_operands = 0x1f000071l in
  if zero_out_operands_mask land code = cmp_with_zero_operands then
    let open BitVec in
    let size = 32 in
    (* get the register *)
    let bv5 = of_int ~size 5 in
    let code_bv = BytesSeq.getbvle ~size code_bs 0 lsr bv5 in
    let bottom5 = of_int ~size 0x0000001f in
    let reg = to_int @@ (code_bv land bottom5) in
    (* get the value *)
    let code_bv = code_bv lsr bv5 in
    let bottom12 = of_int ~size 0x00000fff in
    (* values are 64 bits, spec says imm12 is unsigned *)
    let value = BitVec.zero_extend size @@ (code_bv land bottom12) in
    Some (reg, value)
  else None

(*$}*)

(*$
  ;;
  inject

  let check_cmp (reg, value) = function
    | Some (reg', value') -> reg = reg' && value = BitVec.to_int value'
    | None -> false
*)

(*$T is_cmp'
     (0x7100001fl |> to_be_bytes_seq |> is_cmp' |> check_cmp (0, 0))
     (0xf100009fl |> to_be_bytes_seq |> is_cmp' |> check_cmp (4, 0))
     (0x7100745fl |> to_be_bytes_seq |> is_cmp' |> check_cmp (2, 0x1d))
     (0x7172695fl |> to_be_bytes_seq |> is_cmp' |> check_cmp (10, 0xc9a))
     (0xf118171fl |> to_be_bytes_seq |> is_cmp' |> check_cmp (24, 0x605))
     (0xf100313fl |> to_be_bytes_seq |> is_cmp'	|> check_cmp (9, 0xc))
     (0x6100313fl |> to_be_bytes_seq |> is_cmp'	|> Option.is_none)
     (0xf600313fl |> to_be_bytes_seq |> is_cmp'	|> Option.is_none)
     (0xf1a0313fl |> to_be_bytes_seq |> is_cmp'	|> Option.is_none)
     (0xf10031cfl |> to_be_bytes_seq |> is_cmp'	|> Option.is_none)
     (0xf100313el |> to_be_bytes_seq |> is_cmp'	|> Option.is_none)
*)
*)
let is_cmp _code_bs = None (*
  Option.(
    let* (reg, value) = is_cmp' code_bs in
    (* fixup types *)
    match Reg.of_int reg with Some reg -> Some (reg, value) | None -> Raise.unreachable ())*)

let is_bl _code_bs = None (*
  assert (BytesSeq.length code_bs = 4);
  let (( = ), ( land )) = Int32.(( = ), logand) in
  let code = BytesSeq.get32be code_bs 0 in
  let zero_out_operands_mask = 0x000000fcl in
  let bl_with_zero_operands = 0x00000094l in
  if zero_out_operands_mask land code = bl_with_zero_operands then
    let open BitVec in
    let size = 32 in
    let code_bv = BytesSeq.getbvle ~size code_bs 0 in
    (* get the imm26 *)
    let bottom26 = of_int ~size 0x03ffffff in
    let two = of_int ~size 2 in
    (* values are 64 bits, spec says imm26 is multiplied by 4
       (shifted left by 2) and sign extended *)
    Some (BitVec.sign_extend 64 @@ BitVec.extract 0 27 ((code_bv land bottom26) lsl two))
  else None*)

(*$
  ;;
  inject

  let check_bl y = function None -> false | Some x -> x |> BitVec.to_z |> Z.equal (Z.of_int64 y)
*)

(*$T is_bl
     (0x96030000l |> to_be_bytes_seq |> is_bl |> check_bl 0xfffffffff80c0000L)
     (0x95000009l |> to_be_bytes_seq |> is_bl |> check_bl 0x4000024L)
     (0x35000009l |> to_be_bytes_seq |> is_bl |> Option.is_none)
     (0x93000009l |> to_be_bytes_seq |> is_bl |> Option.is_none)
     (0x9b000009l |> to_be_bytes_seq |> is_bl |> Option.is_none)
*)
