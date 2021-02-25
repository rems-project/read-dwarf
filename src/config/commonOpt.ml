(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   *)
(*  BSD 2-Clause License. This license does not apply to files outside these        *)
(*  directories.                                                                    *)
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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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

(** This module provide support for common command line option to be used across
    multiple subcomands.

    It also provide some {{!Utils}utilities} on the command line.*)

open Cmdliner
open CmdlinerHelper

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Exit codes } *)

(** Description of read-dwarf exit codes. May need to be updated *)
let exits =
  let doc = "on external errors, (Parsing error, Typing error, ...)." in
  let doc2 = "on non-exception internal errors like assertion failed." in
  Term.exit_info ~doc 1 :: Term.exit_info ~doc:doc2 2 :: Term.default_exits

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Config options }

    This section is to manage the configuration file.
    See {!Config.File} to see how the configuration file works
*)

(** Passing [--config=FILE] on the CLI will setup the {!Config.File} module to
    load that file as the configuration file.

    For test executables, you will have to call {!File.ensure_loaded} directly. *)

let config =
  let doc =
    Printf.sprintf "Overrides the default location of the configuration file (%s)"
      Default.config_file
  in
  let env = Arg.env_var "READ_DWARF_CONFIG" ~doc in
  let doc = "Configuration file path" in
  Term.(
    const File.ensure_loaded
    $ Arg.(
        value
        & opt non_dir_file Default.config_file
        & info ["c"; "config"] ~env ~docv:"CONFIG_TOML" ~doc))

let arch_val _config archopt = Option.value_fun archopt ~default:File.get_arch_name

let arch_opt =
  let doc =
    "Override architecture to be analysed. If an ELF is provided this option is ignored and the \
     architecture of the ELF is used instead"
  in
  Arg.(value & opt (some Arch.conv) None & info ["a"; "arch"] ~docv:"ARCH" ~doc)

let arch = Term.(const arch_val $ config $ arch_opt)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Isla options } *)

(** The isla_client path *)
let isla_client_ref = ref "isla-client"

let isla_client =
  let doc = "Overrides the default isla position (named isla-client)" in
  let env = Arg.env_var "ISLA_CLIENT" ~doc in
  let doc = "isla-client location" in
  setter isla_client_ref
    Arg.(value & opt string "isla-client" & info ["isla"] ~env ~docv:"ISLA_CLIENT_PATH" ~doc)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Z3 options } *)

(** The z3 command *)
let z3_ref = ref "z3"

(** The z3 option *)
let z3 =
  let doc = "Overrides the default z3 position" in
  let env = Arg.env_var "Z3_PATH" ~doc in
  let doc = "z3 location" in
  setter z3_ref Arg.(value & opt string "z3" & info ["z3"] ~env ~docv:"Z3_PATH" ~doc)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Common option list } *)

(** The list of common options. Almost all sub-commands should use this. *)
let comopts = [isla_client; z3; Logs.term; config]
