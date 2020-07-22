(* The documentation is in the mli file *)

type 'a printf = ('a, out_channel, unit) format -> 'a

type ('a, 'b) printf_fatal = ('a, out_channel, unit, 'b) format4 -> 'a

type level = Base | Err | Warn | Info | Debug

let default_level = Warn

let pp_level =
  let open PP in
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
      if ConfigPre.enable_backtrace then Printexc.print_raw_backtrace o stack;
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
  let id = register S.str

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
