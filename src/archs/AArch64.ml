(** This module provides the {!ArchSig.Arch} interface for the AArch64 architecture *)

open ArchSig

open Logs.Logger (struct
  let str = "AArch64"
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Internal data } *)

(** All the internal data that should be loaded *)
type t = { reg_map : dwarf_reg_map }

(** Is the module loaded *)
let data = ref None

let get_data () =
  match !data with
  | Some d -> d
  | None -> fatal "Someone tried to use AArch64 module before loading it"

let gen_reg_map () =
  let res = Array.make 32 Reg.empty_path in
  for i = 0 to 30 do
    res.(i) <- [Reg.add (Printf.sprintf "R%d" i) (Reg.Plain (Isla.Ty_BitVec 64))]
  done;
  (* TODO find a way of make the EL2 part of the config *)
  res.(31) <- [Reg.add "SP_EL2" (Reg.Plain (Isla.Ty_BitVec 64))];
  res

let gen_t () =
  let reg_map = gen_reg_map () in
  { reg_map }

(** Initialize the module *)
let actual_init () =
  if !data = None then data := Some (gen_t ()) else warn "AArch64 was loaded multiple times"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 API }

    The implementation of the {!ArchSig.Arch} API. Go there for documentation *)

let supports = function Type.AARCH64 -> true | _ -> false

let init = function
  | Type.AARCH64 -> actual_init ()
  | a ->
      Raise.fail "This version was compiled with only aarch64 support. %t is not supported"
        PP.(tos ArchSig.Type.pp a)

let initialized () = if !data <> None then Some Type.AARCH64 else None

let module_name = "aarch64"

let loaded_name = "aarch64"

let dwarf_reg_map () = (get_data ()).reg_map
