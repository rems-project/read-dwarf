(** This module provides the {!ArchSig.Arch} interface for the AArch64 architecture

    All the ABI related function are derived from the official
    "Procedure Call Standard for the ARM 64-bit Architecture" (ref: AAPCS64)
*)

open ArchSig

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal register info } *)

let nop_int : Int32.t = 0xd503201fl

(** All the internal data that should be loaded *)
type t = {
  config : ConfigFile.Arch.t;
  reg_map : dwarf_reg_map;
  local_regs : bool Reg.Map.t;
  nop : BytesSeq.t;
  pc : Reg.t;
}

(** Is the module loaded *)
let data = ref None

(** Get the loaded data, fails if the module wasn't loaded with {!init} *)
let get_data () =
  match !data with
  | Some d -> d
  | None -> fatal "Someone tried to use AArch64 module before loading it"

(** Generates the register map *)
let gen_reg_map () =
  let res = Array.init 32 (fun i -> Reg.ensure_add [Printf.sprintf "R%d" i] (Ast.Ty_BitVec 64)) in
  res.(31) <- Reg.ensure_add ["SP_EL2"] (Ast.Ty_BitVec 64);
  res

(** Generates the Register map of local registers *)
let gen_local (dwarfregs : dwarf_reg_map) : bool Reg.Map.t =
  (* The PC is local in a certain sense, I don't want to match it in any way *)
  let pc = Reg.ensure_add ["_PC"] (Ast.Ty_BitVec 64) in
  (* According to section 5.1, if we exclude any FP registers,
     the only local register seem to be R0-R30, SP and
     flags N,Z,C,V of PSTATE *)
  let n = Reg.ensure_add ["PSTATE"; "N"] (Ast.Ty_BitVec 1) in
  let z = Reg.ensure_add ["PSTATE"; "Z"] (Ast.Ty_BitVec 1) in
  let c = Reg.ensure_add ["PSTATE"; "C"] (Ast.Ty_BitVec 1) in
  let v = Reg.ensure_add ["PSTATE"; "V"] (Ast.Ty_BitVec 1) in
  let local = Reg.Map.init (fun _ -> false) in
  Array.iter (fun reg -> Reg.Map.set local reg true) dwarfregs;
  Reg.Map.set local pc true;
  Reg.Map.set local n true;
  Reg.Map.set local z true;
  Reg.Map.set local c true;
  Reg.Map.set local v true;
  local

(** Generate the nop instruction *)
let gen_nop () =
  let nop = Bytes.create 4 in
  Bytes.set_int32_le nop 0 nop_int;
  BytesSeq.of_bytes nop

let gen_pc () = Reg.of_string "_PC"

(** Generate the internal arch data *)
let gen_t () =
  let config = ConfigFile.get_arch_config Type.AARCH64 in
  let reg_map = gen_reg_map () in
  let local_regs = gen_local reg_map in
  let nop = gen_nop () in
  let pc = gen_pc () in
  { config; reg_map; local_regs; nop; pc }

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
  reg_num : int;  (** The number of register used as regular arguments *)
  reg_types : Ctype.t array;  (** The types of those register (length reg_type = reg_num *)
  stack_length : int;  (** The number of stack bytes used to store arguments *)
  stack_fragment : Fragment.t;  (** The fragment with their types *)
  ret_pointer : Ctype.t option;  (** The optional return pointer type *)
}

(** TODO move that elsewhere *)
let roundupto x align = (x + align - 1) / align * align

(** Get an {!abi_repr} from an API ({!func_api})

    Follows the official "Procedure Call Standard for the ARM 64-bit Architecture"
*)
let get_abi_repr api : abi_repr =
  let open Ctype in
  (* Replace array by pointer to Single of the array *)
  let cdecay arg = if is_array arg then of_frag (Single arg) else arg in
  let args = List.map cdecay api.args in

  (* Section B of 5.4.2 *)
  let preprocess_arg argt =
    (* B.1 : No dynamically sized argument *)
    (* B.2 : No floating point values *)
    if sizeof argt > 16 then
      (* B.3 *)
      let fragment = Single argt in
      (8, of_frag ~restrict:true fragment)
    else if is_composite argt then
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
        if is_scalar arg && size <= 8 then begin
          reg_types.(!vNGRN) <- arg;
          incr vNGRN;
          raise Allocated
        end;
        (* Item C.8 : We don't support 16 bytes alignement *)
        (* Item C.9 : We don't support 16 bytes integers *)
        (* Item C.10 *)
        if is_composite arg && size / 8 <= 8 - !vNGRN then begin
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
        if is_composite arg then begin
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

let is_local (reg : Reg.t) =
  let data = get_data () in
  Reg.Map.get data.local_regs reg

let nop () = (get_data ()).nop

let pc () = (get_data ()).pc

let get_abi api =
  let open Ctype in
  let data = get_data () in
  let repr = get_abi_repr api in
  let sp = data.reg_map.(31) in
  let r30 = data.reg_map.(30) in
  let ret_pointer_reg = data.reg_map.(8) in
  let init (state : State.t) =
    let state = State.copy_if_locked state in
    debug "Map:\n%t" (PP.topi (Reg.Map.pp PP.bool) data.local_regs);
    Reg.Map.iteri
      (fun reg b ->
        if b then begin
          debug "register to be reset %t" (PP.top Reg.pp reg);
          State.reset_reg state reg
        end)
      data.local_regs;
    for i = 0 to repr.reg_num - 1 do
      let tval = State.Tval.of_var ~ctyp:repr.reg_types.(i) (Arg i) in
      State.set_reg state data.reg_map.(i) tval
    done;
    ( match repr.ret_pointer with
    | None -> ()
    | Some ctyp -> State.set_reg state ret_pointer_reg (State.Tval.of_var ~ctyp RetArg)
    );
    let stack_frag_id = Fragment.Env.add_frag ~frag:repr.stack_fragment state.fenv in
    State.set_reg_type state sp (Ctype.of_frag @@ FreeFragment stack_frag_id);
    State.set_reg state r30
      (State.Tval.of_var ~ctyp:(Ctype.of_frag_somewhere Ctype.Global) RetAddr);
    let sp_exp = State.Exp.of_reg state.id sp in
    (* Assert that Sp is 16 bytes aligned *)
    State.push_assert state Ast.Op.(extract 3 0 sp_exp = bits_int ~size:4 0);
    (* Assert that Sp has zero top bits *)
    State.push_assert state Ast.Op.(extract 63 52 sp_exp = bits_int ~size:12 0);
    (* Assert that there is enough stack space *)
    State.push_assert state Ast.Op.(comp Ast.Bvuge sp_exp (bits_int ~size:64 0x1000));
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
  Cmd.output_string [|assembler; "-g"; "-o"; obj_file|] (prefix ^ instr ^ "\n");
  Cmd.cmd [|linker; obj_file; "--entry=instr"; "-o"; elf_file|];
  Sys.remove obj_file;
  elf_file
