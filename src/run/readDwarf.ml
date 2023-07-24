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

(** This module implements the [rd] subcommand. This was to original meaning of "read-dwarf",
    and the first command.

    Together with {{!Run.FuncRD}[run-func-rd]}, this is the main user of the {!Analyse} code. *)

open Cmdliner
open CmdlinerHelper
open Config.CommonOpt

let dry_run =
  let doc = "dry run: " in
  setter Analyse.Globals.copy_sources_dry_run
    Arg.(value & flag & info ["dry-run"] ~docv:"DRY_RUN" ~doc)

let skylight =
  let doc = "skylight" in
  setter Analyse.Globals.skylight Arg.(value & flag & info ["skylight"] ~docv:"SKYLIGHT" ~doc)

let comp_dir =
  let doc = "Path to root directory of source files, overriding DWARF comp_dir" in
  setter Analyse.Globals.comp_dir
    Arg.(value & opt (some string) None & info ["comp-dir"] ~docv:"COMP_DIR" ~doc)

let clip_binary =
  let doc = "clip binary to first 1000 instructions" in
  setter Analyse.Globals.clip_binary Arg.(value & flag & info ["clip-binary"] ~doc)

let no_vars =
  let doc = "Do not print variable information at each instruction" in
  setter Analyse.Globals.show_vars Term.(const not $ Arg.(value & flag & info ["no-vars"] ~doc))

let no_cfa =
  let doc = "Do not print CFA information" in
  setter Analyse.Globals.show_cfa Term.(const not $ Arg.(value & flag & info ["no-cfa"] ~doc))

let no_source =
  let doc = "Do not print source lines at each instruction" in
  setter Analyse.Globals.show_source
    Term.(const not $ Arg.(value & flag & info ["no-source"] ~doc))

let objdump_d =
  let doc = "File containing result of objdump -d" in
  setter Analyse.Globals.objdump_d
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables =
  let doc = "File containing branch table base addresses and sizes" in
  setter Analyse.Globals.branch_table_data_file
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf =
  let doc = "ELF file whose dwarf is to be dumped" in
  setter Analyse.Globals.elf
    Arg.(value & opt (some non_dir_file) None & info ["elf"] ~docv:"ELF_FILE" ~doc)

let objdump_d2 =
  let doc = "Second: File containing result of objdump -d" in
  setter Analyse.Globals.objdump_d2
    Arg.(value & opt (some non_dir_file) None & info ["objdump-d2"] ~docv:"OBJDUMP_FILE" ~doc)

let branch_tables2 =
  let doc = "Second: File containing branch table base addresses and sizes" in
  setter Analyse.Globals.branch_table_data_file2
    Arg.(
      value
      & opt (some non_dir_file) None
      & info ["branch-tables2"] ~docv:"BRANCH_TABLES_FILE" ~doc)

let elf2 =
  let doc = "Second: ELF file whose dwarf is to be dumped" in
  setter Analyse.Globals.elf2
    Arg.(value & opt (some non_dir_file) None & info ["elf2"] ~docv:"ELF_FILE" ~doc)

let qemu_log =
  let doc = "QEMU log file" in
  setter Analyse.Globals.qemu_log
    Arg.(value & opt (some non_dir_file) None & info ["qemu-log"] ~docv:"QEMU_LOG_FILE" ~doc)

let out_file =
  let doc = "file for single-file output (optional)" in
  setter Analyse.Globals.out_file
    Arg.(value & opt (some string) None & info ["o"; "out"] ~docv:"OUT_FILE" ~doc)

let out_dir =
  let doc = "directory for multiple-file output (optional)" in
  setter Analyse.Globals.out_dir
    Arg.(value & opt (some string) None & info ["out-dir"] ~docv:"OUT_DIR" ~doc)

let cfg_dot_file =
  let doc = "File to output CFG dot to" in
  setter Analyse.Globals.cfg_dot_file
    Arg.(value & opt (some string) None & info ["cfg-dot-file"] ~docv:"CFG_DOT_FILE" ~doc)

let cfg_source_nodes =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Analyse.Globals.cfg_source_nodes
    Arg.(value & opt (some string) None & info ["cfg-source-nodes"] ~docv:"CFG_SOURCE_NODES" ~doc)

let cfg_source_nodes2 =
  let doc = "Optional space-separated list of node labels to start the CFG from" in
  setter Analyse.Globals.cfg_source_nodes2
    Arg.(
      value & opt (some string) None & info ["cfg-source-nodes2"] ~docv:"CFG_SOURCE_NODES2" ~doc)

let html =
  let doc = "Enables html output" in
  setter Analyse.Globals.ppmode
    Term.(
      const (fun b -> if b then Analyse.Types.Html else Analyse.Types.Ascii)
      $ Arg.(value & flag & info ["h"; "html"] ~doc))

let morello =
  let doc = "Enable Morello support" in
  setter Analyse.Globals.morello
    Arg.(value & flag & info ["morello"] ~docv:"MORELLO" ~doc)

let suppress_stuff =
  let doc = "Suppress some stuff in html output" in
  setter Analyse.Globals.suppress_stuff
    Arg.(value & flag & info ["suppress_stuff"] ~docv:"SUPPRESS_STUFF" ~doc)


  
let options =
  [
    skylight;
    dry_run;
    comp_dir;
    no_vars;
    no_cfa;
    no_source;
    elf;
    objdump_d;
    branch_tables;
    elf2;
    objdump_d2;
    branch_tables2;
    qemu_log;
    clip_binary;
    out_file;
    out_dir;
    cfg_dot_file;
    cfg_source_nodes;
    cfg_source_nodes2;
    html;
    morello ;
    suppress_stuff;
    Logs.term;
  ]

let full_term = Term.(func_options options Analyse.process_file $ const ())

let info =
  let doc =
    "Dumps an ELF file in the read-dwarf format. DWARF information is interleaved with the \
     result of the objdump, so one can see how the dwarf information is positioned compared to \
     the assembly. It will also try to read the source file to interleave the original source \
     code."
  in
  Cmd.(info "rd" ~doc ~exits)

let command = (full_term, info)
