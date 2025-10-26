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

open Cmdliner
open Config.CommonOpt

module Default = struct
  (** Default action to run when no command is set *)
  let action _arch = print_endline "Error: read-dwarf, no command specified. Use --help for help"

  (** Global documentation string and name *)
  let info =
    let doc = "Parse dwarf information and use isla to run assembly" in
    Cmd.(info "read-dwarf" ~doc ~exits)

  (** Default command *)
  let command = (Term.(CmdlinerHelper.func_options comopts action $ arch), info)
end

(** List of all non-default commands *)
let commands =
  [
    Run.ReadDwarf.command;
    Isla.Test.command;
    Other_cmds.DumpSym.command;
    Isla.Server.Cmd.command;
    Run.BB.command;
    Other_cmds.DumpDwarf.command;
    Cache.Cmd.command;
    Run.Func.command;
    Run.Instr.command;
    Run.Block.command;
    (* Run.FuncRD.command; *)
    Other_cmds.CopySourcesCmd.command;
    (* BranchTable.command; *)
    Z3.Test.command;
  ]

let _ = Printexc.record_backtrace Config.enable_backtrace

(* Other architectures are unsupported for now. Remove this only when you are explicitly
   working on making other architecture work with read-dwarf *)
let _ = assert ("aarch64" = Arch.module_name)

(* TODO allow to set the seed in compile time or run time config for debugging *)
let _ = Random.self_init ()

(** Main entry point *)
let _ = grouped_exe commands Default.command