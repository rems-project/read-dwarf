(** This module provides the {!ArchSig.Arch} interface for the AArch64 architecture

    All the ABI related function are derived from the official
    "Procedure Call Standard for the ARM 64-bit Architecture" (ref: AAPCS64)
*)

open ArchSig

open Logs.Logger (struct
  let str = "AArch64"
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal register info } *)

let nop_int : Int32.t = 0xd503201fl

(** All the internal data that should be loaded *)
type t = { reg_map : dwarf_reg_map; local_regs : bool Reg.Map.t; nop : BytesSeq.t }

(** Is the module loaded *)
let data = ref None

(** Get the loaded data, fails if the module wasn't loaded with {!init} *)
let get_data () =
  match !data with
  | Some d -> d
  | None -> fatal "Someone tried to use AArch64 module before loading it"

(** Generates the register map *)
let gen_reg_map () =
  let res = Array.make 32 Reg.empty_path in
  for i = 0 to 30 do
    res.(i) <- [Reg.add (Printf.sprintf "R%d" i) (Reg.Plain (Ast.Ty_BitVec 64))]
  done;
  (* TODO find a way of make the EL2 part of the config *)
  res.(31) <- [Reg.add "SP_EL2" (Reg.Plain (Ast.Ty_BitVec 64))];
  res

(** Generates the Register map of local registers *)
let gen_local (dwarfregs : dwarf_reg_map) : bool Reg.Map.t =
  (* According to section 5.1, if we exclude any FP registers,
     the only local register seem to be R0-R30, SP and
     flags N,Z,C,V of PSTATE *)
  let pstate = Reg.make_struct () in
  let n = Reg.add_field pstate "N" (Reg.Plain (Ast.Ty_BitVec 1)) in
  let z = Reg.add_field pstate "Z" (Reg.Plain (Ast.Ty_BitVec 1)) in
  let c = Reg.add_field pstate "C" (Reg.Plain (Ast.Ty_BitVec 1)) in
  let v = Reg.add_field pstate "V" (Reg.Plain (Ast.Ty_BitVec 1)) in
  let pstate = Reg.add "PSTATE" (Reg.Struct pstate) in
  let res = Reg.Map.init (fun _ -> false) in
  Array.iter (fun path -> Reg.Map.set res path true) dwarfregs;
  Reg.Map.set res [pstate; n] true;
  Reg.Map.set res [pstate; z] true;
  Reg.Map.set res [pstate; c] true;
  Reg.Map.set res [pstate; v] true;
  res

(** Generate the nop instruction *)
let gen_nop () =
  let nop = Bytes.create 4 in
  Bytes.set_int32_le nop 0 nop_int;
  BytesSeq.of_bytes nop

(** Generate the internal arch data *)
let gen_t () =
  let reg_map = gen_reg_map () in
  let local_regs = gen_local reg_map in
  let nop = gen_nop () in
  { reg_map; local_regs; nop }

(** Initialize the module. Internal function *)
let actual_init () =
  info "Loading AArch64 module";
  if !data = None then data := Some (gen_t ()) else warn "AArch64 was loaded multiple times"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal ABI implementation } *)

(** The internal ABI representation *)
type abi_repr = {
  reg_num : int;
  reg_types : Ctype.t array;
  stack_length : int;
  stack_fragment : Fragment.t;
  ret_pointer : Ctype.t option;
}

(** TODO move that elsewhere *)
let roundupto x align = (x + align - 1) / align * align

(** Get an {!abi_repr} from an API ({!func_api})

    Follows the official "Procedure Call Standard for the ARM 64-bit Architecture"
*)
let get_abi_repr api : abi_repr =
  let open Ctype in
  (* Replace array by pointer to Single of the array *)
  let cdecay arg = if is_array arg.unqualified then of_frag (Single arg) else arg in
  let args = List.map cdecay api.args in

  (* Section B of 5.4.2 *)
  let preprocess_arg argt =
    (* B.1 : No dynamically sized argument *)
    (* B.2 : No floating point values *)
    if sizeof argt > 16 then
      (* B.3 *)
      let fragment = Single argt in
      (8, of_frag ~restrict:true fragment)
    else if is_composite argt.unqualified then
      (* B.4 *)
      let size = sizeof argt in
      (roundupto size 8, argt)
    else (sizeof argt, argt)
  in
  let ppargs = List.map preprocess_arg args in

  (* Section C of 5.4.2 *)
  let process_args args : abi_repr =
    (* This algorithm is implemented exactly as described in the document
       It could be simplified, but I think it's better to copy the reference
       literally *)
    (* The mutable variable is args *)
    let vNGRN = ref 0 in
    let vNSAA = ref 0 in
    let reg_types = Array.make 8 (machine 8) in
    let stack_fragment = ref Fragment.empty in
    let allocate_stack t = stack_fragment := Fragment.add !stack_fragment !vNSAA t in
    let process_arg (size, arg) =
      let exception Allocated in
      try
        (* From C.1 to C.6: We ignore floating point values *)
        (* Item C.7 *)
        if is_scalar arg.unqualified && size <= 8 then begin
          reg_types.(!vNGRN) <- arg;
          incr vNGRN;
          raise Allocated
        end;
        (* Item C.8 : We don't support 16 bytes alignement *)
        (* Item C.9 : We don't support 16 bytes integers *)
        (* Item C.10 *)
        if is_composite arg.unqualified && size / 8 <= 8 - !vNGRN then begin
          if size <= 8 then reg_types.(!vNGRN) <- arg
          else
            (* If struct is bigger than one register, then don't type the registers *)
            for i = !vNGRN to !vNGRN + (size / 8) - 1 do
              reg_types.(i) <- machine 8
            done;
          vNGRN := !vNGRN + (size / 8);
          raise Allocated
        end;
        (* Item C.11 *)
        vNGRN := 8;
        (* Item C.12 : No 16 bytes alignement *)
        vNSAA := roundupto !vNSAA 8;
        (* Item C.13 *)
        if is_composite arg.unqualified then begin
          allocate_stack arg;
          vNSAA := !vNSAA + size;
          raise Allocated
        end;
        (* Item C.14 *)
        let nsize = if size < 8 then 8 else size in
        (* Item C.15 *)
        allocate_stack arg;
        vNSAA := !vNSAA + size
      with Allocated -> ()
    in
    List.iter process_arg args;
    {
      reg_num = !vNGRN;
      reg_types;
      stack_length = !vNSAA;
      stack_fragment = !stack_fragment;
      ret_pointer = None;
    }
  in
  let irepr = process_args ppargs in

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
(** {1 ArchSigAPI }

    The implementation of the {!ArchSig.S} API. Go there for documentation *)

let supports = function Type.AARCH64 -> true | _ -> false

let init = function
  | Type.AARCH64 -> actual_init ()
  | a ->
      Raise.fail "This version was compiled with only aarch64 support. %t is not supported"
        PP.(tos ArchSig.Type.pp a)

let initialized () = if !data <> None then Some Type.AARCH64 else None

let module_name = "aarch64"

let loaded_name = "aarch64"

let address_size = 52

let dwarf_reg_map () = (get_data ()).reg_map

let is_local (reg : Reg.path) =
  let data = get_data () in
  Reg.Map.get_or ~value:false data.local_regs reg

let nop () = (get_data ()).nop

let get_abi api =
  let open Ctype in
  let data = get_data () in
  let repr = get_abi_repr api in
  let sp = data.reg_map.(31) in
  let ret_pointer_reg = data.reg_map.(8) in
  let init (state : State.t) =
    let state = State.copy_extend state in
    debug "Map:\n%t" (PP.topi (Reg.Map.pp PP.bool) data.local_regs);

    Reg.Map.iteri
      (fun path b ->
        if b then debug "path to be reset %t" (PP.topi Reg.pp_path path);
        State.reset_reg state path)
      data.local_regs;
    for i = 0 to repr.reg_num - 1 do
      let tval = State.make_tval ~ctyp:repr.reg_types.(i) (State.Var.to_exp @@ Arg i) in
      State.set_reg state data.reg_map.(i) tval
    done;
    ( match repr.ret_pointer with
    | None -> ()
    | Some ctyp ->
        State.set_reg state ret_pointer_reg (State.make_tval ~ctyp (State.Var.to_exp @@ RetArg))
    );
    let stack_frag_id = Fragment.Env.add_frag ~frag:repr.stack_fragment state.fenv in
    State.set_reg_type state sp (Ctype.of_frag @@ FreeFragment stack_frag_id);
    State.lock state;
    state
  in
  { init }

let assemble_to_elf instr =
  let assembler = ConfigPre.aarch64_toolchain ^ "-as" in
  let linker = ConfigPre.aarch64_toolchain ^ "-ld" in
  let num = Random.bits () in
  let obj_file = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "%d.o" num) in
  let elf_file = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "%d.elf" num) in
  let prefix = ".global instr\n.type instr, @function\n.size instr,4\ninstr:" in
  Cmd.output_string [|assembler; "-g"; "-o"; obj_file|] (prefix ^ instr ^ "\n");
  Cmd.cmd [|linker; obj_file; "--entry=instr"; "-o"; elf_file|];
  Sys.remove obj_file;
  elf_file
