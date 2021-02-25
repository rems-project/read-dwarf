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

(* The documentation is in the mli file *)

type 'a printf = ('a, out_channel, unit) format -> 'a

type ('a, 'b) printf_fatal = ('a, out_channel, unit, 'b) format4 -> 'a

type level = Base | Err | Warn | Info | Debug

let default_level = Warn

let pp_level =
  let open Pp in
  function
  | Base -> empty
  | Err -> !^"[Error]"
  | Warn -> !^"[Warn]"
  | Info -> !^"[Info]"
  | Debug -> !^"[Debug]"

let level_to_string = function
  | Base -> "Base"
  | Err -> "Error"
  | Warn -> "Warn"
  | Info -> "Info"
  | Debug -> "Debug"

let level_of_string = function
  | "Base" -> Base
  | "Error" -> Err
  | "Warn" -> Warn
  | "Info" -> Info
  | "Debug" -> Debug
  | s -> Raise.inv_arg "%s is not a valid log level" s

let level_to_header = function
  | Base -> ""
  | Err -> "[Error]"
  | Warn -> "[Warn]"
  | Info -> "[Info]"
  | Debug -> "[Debug]"

let level_fmt f t = t |> level_to_string |> Format.pp_print_string f

let level_conv =
  let docv = "Log level: Base, Error, Warn, Info or Debug" in
  let parser a = try Ok (level_of_string a) with Invalid_argument s -> Error (`Msg s) in
  Cmdliner.Arg.conv ~docv (parser, level_fmt)

let lvl_list = [Base; Err; Warn; Info; Debug]

let outputs = Hashtbl.create 5

let _ =
  Hashtbl.add outputs Base stdout;
  Hashtbl.add outputs Err stderr;
  Hashtbl.add outputs Warn stderr;
  Hashtbl.add outputs Info stderr;
  Hashtbl.add outputs Debug stderr

let channel = Hashtbl.find outputs

let set_stdout_level lvl =
  List.iter
    (fun l ->
      if l <= lvl then Hashtbl.replace outputs l stdout else Hashtbl.replace outputs l stderr)
    lvl_list

let baselog name lvl fmt =
  let out = channel lvl in
  Printf.fprintf out "[%s]%s: " name (level_to_header lvl);
  Printf.kfprintf
    (fun o ->
      Printf.fprintf o "\n";
      flush o)
    out fmt

let baselog_fatal ~code name lvl fmt =
  let stack = Printexc.get_callstack 50 in
  let out = channel lvl in
  Printf.fprintf out "[%s]%s Fatal: " name (level_to_header lvl);
  Printf.kfprintf
    (fun o ->
      Printf.fprintf o "\n";
      flush o;
      Printexc.print_raw_backtrace o stack;
      flush o;
      exit code)
    out fmt

let loggers : (string, level) IdMap.t = IdMap.make ()

let register (name : string) =
  try IdMap.add loggers name default_level
  with IdMap.Exists ->
    Printf.eprintf "Logger %s is registered twice, fix and recompile\n" name;
    exit 125

let mainlog i lvl fmt =
  if lvl <= IdMap.geti loggers i then
    let name = IdMap.of_ident loggers i in
    baselog name lvl fmt
  else Printf.ifprintf stdout fmt

let mainlog_fatal ~code i lvl fmt =
  let name = IdMap.of_ident loggers i in
  baselog_fatal ~code name lvl fmt

let set_default_level lvl = IdMap.fill_all loggers lvl

let set_level name lvl =
  try IdMap.setk loggers name lvl
  with Not_found -> failwith (Printf.sprintf "Logging module %s not found" name)

module type String = sig
  val str : string
end

module Logger (S : String) = struct
  let fixup str = str |> String.split_on_char '_' |> List.filter (( <> ) "") |> String.concat "."

  let id = register @@ fixup S.str

  let set_level lvl = IdMap.seti loggers id lvl

  let get_level () = IdMap.geti loggers id

  let log lvl fmt = mainlog id lvl fmt

  let log_fatal ~code lvl fmt = mainlog_fatal ~code id lvl fmt

  let base fmt = log Base fmt

  let fail fmt = mainlog_fatal ~code:1 id Base fmt

  let err fmt = log Err fmt

  let fatal fmt = mainlog_fatal ~code:2 id Err fmt

  let warn fmt = log Warn fmt

  let info fmt = log Info fmt

  let debug fmt = log Debug fmt

  let has_debug () = get_level () >= Debug
end

open Cmdliner

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:logs Logging options }

    This section provide option to control the {!output.*)

let quiet_ref = ref false

(** Passing [--quiet] on the CLI will disable all {!level} except {!Base}
    for all modules *)
let quiet =
  let doc = "Remove all errors and warnings from the output" in
  Arg.(value & flag & info ["q"; "quiet"] ~doc)

(** Passing [-v] or [--verbose] on the CLI will enable {!Info} level logs.
    If passed twice of more, it will also enable {!Debug} logs.

    This is for all modules *)
let verbose =
  let doc = "Log more stuff. When set twice, output all debugging logs" in
  Arg.(value & flag_all & info ["v"; "verbose"] ~doc)

(** Passing [--info=MODULE] on the CLI will set that precise module
    at log level {!Info} *)
let infoopt : string list Term.t =
  let doc = "Set a precise OCaml module in info-logging mode" in
  Arg.(value & opt_all string [] & info ["info"] ~doc ~docv:"MODULE")

(** Passing [--debug=MODULE] on the CLI will set that precise module
    at log level {!Debug} *)
let debug =
  let doc = "Set a precise OCaml module in debug-logging mode" in
  Arg.(value & opt_all string [] & info ["debug"] ~doc ~docv:"MODULE")

(** Passing [--stdout-level=LEVEL] on the CLI will redirect all log message
    below or equal to that level to [stdout] instead of [stderr] *)
let stdout_level =
  let doc = "Set the log level below which the message go on stdout" in
  Arg.(value & opt level_conv Base & info ["stdout-level"] ~doc ~docv:"LEVEL")

let process_opts quiet verbose info debug stdout_level =
  if quiet then set_default_level Base;
  if quiet then quiet_ref := true;
  begin
    match verbose with
    | [] -> ()
    | [true] -> set_default_level Info
    | _ -> set_default_level Debug
  end;
  List.iter (fun name -> set_level name Info) info;
  List.iter (fun name -> set_level name Debug) debug;
  set_stdout_level stdout_level

let term = Term.(const process_opts $ quiet $ verbose $ infoopt $ debug $ stdout_level)
