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

(** This module adds some code that is related to the Architecture specific
    modules but is in itself architecture independent. *)

include Sig

(** Ensure that the right architecture type is loaded *)
let ensure_loaded (at : Config.Arch.t) =
  match initialized () with
  | Some at' ->
      if at' != at then
        Raise.fail "ensure_loaded: required architecture is %s but loaded architecture is %s"
          (Config.Arch.to_string at) (Config.Arch.to_string at')
  | None -> init at

let pp_api (api : func_api) =
  let open Pp in
  record "api"
    [
      ("args", api.args |> List.mapi (fun i t -> (int i, Ctype.pp t)) |> mapping "");
      ("ret", Option.fold ~none:!^"none" ~some:Ctype.pp api.ret);
    ]

(** Get the initialized architecture type. Fails ([Failure]) if not architecture was loaded *)
let get () = match initialized () with Some a -> a | None -> Raise.fail "No architecture loaded"

(** Get the configuration for the initialized architecture *)
let get_config () = Config.File.get_arch_config (get ())

(** Get the Isla configuration for the initialized architecture *)
let get_isla_config () = Config.File.get_isla_config (get ())

(** Load the architecture of this {!Elf.File} *)
let load_elf_arch (elf : Elf.File.t) =
  match elf.machine with
  | Supp at -> ensure_loaded at
  | Other i -> Raise.fail "The ELF architecture %i is not supported by this program" i
