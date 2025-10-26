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

(*****************************************************************************)
(**       look up address in DWARF frame info                                *)

(*****************************************************************************)

open Utils
open ElfTypes
open ControlFlowTypes

let aof ((a : natural), (_cfa : string), (_regs : (string * string) list)) = a

(* TODO does Sym.Ordered work as we want? *)
let rec f (aof : 'b -> natural) (a : natural) (last : 'b option) (bs : 'b list) : 'b option =
  match (last, bs) with
  | (None, []) -> None
  | (Some b', []) -> if Sym.Ordered.greater_equal a (aof b') then Some b' else None
  | (None, b'' :: bs') -> f aof a (Some b'') bs'
  | (Some b', b'' :: bs') ->
      if Sym.Ordered.less a (aof b') then None
      else if Sym.Ordered.greater_equal a (aof b') && Sym.Ordered.less a (aof b'') then Some b'
      else f aof a (Some b'') bs'

let mk_frame_info test instructions :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array =
  Array.map (function i -> f aof i.i_addr None test.dwarf_semi_pp_frame_info) instructions

let pp_frame_info _m frame_info k : string =
  (* assuming the dwarf_semi_pp_frame_info has monotonically increasing addresses - always true? *)
  match frame_info.(k) with
  | None -> "<no frame info for this address>\n"
  | Some ((a : natural), (cfa : string), (regs : (string * string) list)) ->
      pp_addr a ^ " " ^ "CFA:" ^ cfa ^ " "
      ^ String.concat " " (List.map (function (rname, rinfo) -> rname ^ ":" ^ rinfo) regs)
      ^ "\n"
