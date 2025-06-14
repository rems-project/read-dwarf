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

(** Miscellaneous types and utility functions used throughout the analyse code *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** TODO: Maybe just use Z.t everywhere (it's shorter) *)
type natural = Sym.t

(** machine address *)
type addr = natural

(* hackishly mask out bigint conversion failure *)
let pp_addr (a : natural) = 
  try
    Sym_ocaml.Num.ppf Ml_bindings.hex_string_of_big_int_pad8 a
  with
  | Failure s -> let s' = "Failure: int64_of_big_int " ^ Sym.to_string a in (warn "pp_addr failure: %s" s); s'
  | e -> raise e

(** index into instruction-indexed arrays *)
type index = int

let measure_time = false

(** Print the time this function call took. The string is just for the printed message *)
let time s f x =
  if measure_time then (
    let t1 : Unix.process_times = Unix.times () in
    let y = f x in
    let t2 : Unix.process_times = Unix.times () in
    Printf.printf "time %s user %6.3f  system %6.3f\n" s (t2.tms_utime -. t1.tms_utime)
      (t2.tms_stime -. t1.tms_stime);
    y
  )
  else f x

(** Read all lines of a file *)
let read_file_lines (name : string) : (string array, string) result =
  match Files.(read (input_array input_line)) name with
  | lines -> Ok lines
  | exception Sys_error s -> Error (Printf.sprintf "read_file_lines Sys_error \"%s\"\n" s)

(** escape HTML *)
let html_escape s =
  let escaping = ref true in
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | c -> (
          match c with
          (* truly horrible hackery *)
          | c when c = '@' -> escaping := not !escaping
          | c -> (
              match !escaping with
              | true -> (
                  match c with
                  | '&' -> Buffer.add_string buf "&amp"
                  | '<' -> Buffer.add_string buf "&lt"
                  | '>' -> Buffer.add_string buf "&gt"
                  | '\"' -> Buffer.add_string buf "&quot"
                  | '\'' -> Buffer.add_string buf "&apos"
                  | c -> Buffer.add_char buf c
                )
              | false -> Buffer.add_char buf c
            )
        ))
    s;
  Buffer.contents buf

let esc m s = match m with Types.Ascii -> s | Types.Html -> html_escape s

let sys_command s =
  if !Globals.copy_sources_dry_run then Printf.printf "dry run: %s\n" s
  else
    let exit_code = Sys.command s in
    if exit_code <> 0 then fatal "sys_command %s failed with exit code %d" s exit_code else ()
