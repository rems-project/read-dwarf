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

(** This module is to handle the configuration file of the program.

    For now I expect a TOML format and use the [toml] library from opam. The data structure
    themselves are format agnostic so if we change the TOML library of change of format, only this
    file must be modified. For example to JSON or YAML.

    The top level usage is to first set the {!file} reference which will be done by
    {!CommonOpt.config} using {!ensure_loaded}.
    Then the config can be accessed with the various {!section:acc}
*)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Configuration structure }

    All field of records in this section have a matching line in the checked-in
    [config.toml]. If something is optional here, and should be disabled in the
    default [config.toml], put a commented line there, so that the
    correspondence can be immediately seen.*)

module ArchConf : sig
  (** This module provides the current isla configuration.
       *)
  module Isla : sig
    (** Isla configuration option.
        Everything in here is salient for cache coherency which means that if any option
        if changed here, the whole {!Isla.Cache} is invalidated.
        This is checked with {!digest}.
    *)
    type t = {
      ignored_regs : string list;  (** The list of register to be ignored *)
      arch_file : string;
          (** The name of the architecture.ir file to use.
              This is the file that was compiled with [isla-sail] from
              the sail source. Alternatively this file can be found in the
              {{:https://github.com/rems-project/isla-snapshots}[isla-snapshots]}
              respository. *)
      arch_toml : string;  (** The name of the architecture.toml file to use with Isla. *)
      arch_file_digest : string;
          (** The digest of the file in {!field:arch_file}.
              This will be used instead of {!field:arch_file} by {!digest}. *)
      linearize : string list;
          (** List of sail function to be linearized.
              That means that if there is a control-flow branching in that
              function, instead of generating multiple traces, isla will generate
              a single trace with it-then-else expression *)
      other_opts : string list;
          (** List of other option to pass to isla. Ideally if an option is to
              be easily used, a new field of {!t} should be created, but for
              quick and dirty testing, this field can be used. *)
    }

    (** Produces a digest of the Isla configuration for invalidating
        the {!Isla.Cache} when some configuration parameter changes *)
    val digest : t -> string
  end

  (** The list of all architecture specific configuration options *)
  type t = {
    arch : Arch.t;  (** The architecture targeted by this record *)
    toolchain : string;
        (** The toolchain prefix for using this architecture with
           GNU binutils like [objdump] and [as]. *)
    isla : Isla.t;  (** The isla configuration for this architecture *)
  }
end

module Z3 : sig
  type t = {
    timeout : int option;  (** Timeout for individual requests in milliseconds *)
    memory : int option;  (** Maximum memory, in MegaBytes *)
    simplify_opts : (string * bool) list;  (** List of option for the simplify command *)
  }
end

(** Each architecture specific configuration is specified by a TOML section
    represented by a {!Arch.t} record.
    They are stored in this type *)
type archs_type = (Arch.t, ArchConf.t) Hashtbl.t

type t = {
  arch : Arch.t option;
      (** The default architecture to be choosen when no ELF file is specified on the CLI.
          This is optional, but if supplied, it will override {!Config.arch} *)
  archs : archs_type;  (** All the architecture specific configurations *)
  z3 : Z3.t;  (** The Z3 configuration *)
}

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 File loading }

    This section is about loading the configuration from a TOML file *)

(** Load the configuration file from the specified file name *)
val load : string -> unit

(** If the configuration file is not already loaded, load it from the specified file.*)
val ensure_loaded : string -> unit

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:acc Accessors }

    The section provide various accessor to access directly part of the configuration

    The config must be loaded otherwise {!UnloadedConfig} will be thrown *)

(** This exception will be raised if any of the accessor is called
    while the config is not loaded. *)
exception UnloadedConfig

(** Get all the configuration information *)
val get_config : unit -> t

(** Get the default architecture name to be used if no architecture is implicitly
    or explicitly specified on the CLI *)
val get_arch_name : unit -> Arch.t

(** Get the architecture configuration for a specific architecture.
    To get the architecture configuration of the currently enabled architecture,
    Call {!Arch.get_config} *)
val get_arch_config : Arch.t -> ArchConf.t

(** Get the isla configuration for a specific architecture.
    To get the isla configuration of the currently enabled architecture,
    Call {!Arch.get_isla_config} *)
val get_isla_config : Arch.t -> ArchConf.Isla.t

(** Get the Z3 configuration *)
val get_z3_config : unit -> Z3.t
