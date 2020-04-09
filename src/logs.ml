open PP

type ldoc = unit -> PP.document

type 'a printf = ('a, out_channel, unit) format -> 'a

type ('a, 'b) printf_fatal = ('a, out_channel, unit, 'b) format4 -> 'a

type level = Base | Err | Warn | Info | Debug

let default_level = Warn

let pp_level = function
  | Base -> empty
  | Err -> !^"[Error]"
  | Warn -> !^"[Warn]"
  | Info -> !^"[Info]"
  | Debug -> !^"[Debug]"

let level_to_string = function
  | Base -> ""
  | Err -> "[Error]"
  | Warn -> "[Warn]"
  | Info -> "[Info]"
  | Debug -> "[Debug]"

let channel = function Base -> stdout | _ -> stderr

let baselog name lvl fmt =
  let out = channel lvl in
  Printf.fprintf out "[%s]%s: " name (level_to_string lvl);
  Printf.kfprintf
    (fun o ->
      Printf.fprintf o "\n";
      flush o)
    out fmt

let baselog_fatal ~code name lvl fmt =
  let stack = Printexc.get_callstack 50 in
  let out = channel lvl in
  Printf.fprintf out "[%s]%s Fatal: " name (level_to_string lvl);
  Printf.kfprintf
    (fun o ->
      Printf.fprintf o "\n";
      flush o;
      if ConfigPre.enable_backtrace then Printexc.print_raw_backtrace o stack;
      flush o;
      exit code)
    out fmt

let baselogd name lvl doc =
  let out = channel lvl in
  PP.fprintln out $ prefix 2 1 (brackets !^name ^^ pp_level lvl ^^ colon) doc;
  flush out

let baselogd_fatal ~code name lvl doc =
  let stack = Printexc.get_callstack 50 in
  let out = channel lvl in
  PP.fprintln out $ prefix 2 1 (brackets !^name ^^ pp_level lvl ^^ !^" Fatal:") doc;
  flush out;
  if ConfigPre.enable_backtrace then Printexc.print_raw_backtrace out stack;
  flush out;
  exit code

let loggers : (string, level) IdMap.t = IdMap.make ()

let register (name : string) = IdMap.add loggers name default_level

let mainlog i lvl fmt =
  if lvl <= IdMap.geti loggers i then
    let name = IdMap.of_ident loggers i in
    baselog name lvl fmt
  else Printf.ifprintf stdout fmt

let mainlog_fatal ~code i lvl fmt =
  let name = IdMap.of_ident loggers i in
  baselog_fatal ~code name lvl fmt

let mainlogd i lvl ldoc =
  if lvl <= IdMap.geti loggers i then
    let name = IdMap.of_ident loggers i in
    baselogd name lvl $ ldoc ()
  else ()

let mainlogd_fatal ~code i lvl ldoc =
  let name = IdMap.of_ident loggers i in
  baselogd_fatal ~code name lvl $ ldoc ()

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

  let log lvl fmt = mainlog id lvl fmt

  let log_fatal ~code lvl fmt = mainlog_fatal ~code id lvl fmt

  let base fmt = log Base fmt

  let fail fmt = mainlog_fatal ~code:1 id Base fmt

  let err fmt = log Err fmt

  let fatal fmt = mainlog_fatal ~code:2 id Err fmt

  let warn fmt = log Warn fmt

  let info fmt = log Info fmt

  let debug fmt = log Debug fmt

  (*****************************************************************************)
  (*        PP.document version                                                *)
  (*****************************************************************************)

  let logd lvl ldoc = mainlogd id lvl ldoc

  let logd_fatal ~code lvl ldoc = mainlogd_fatal ~code id lvl ldoc

  let based ldoc = logd Base ldoc

  let faild ldoc = mainlogd_fatal ~code:1 id Base ldoc

  let errd ldoc = logd Err ldoc

  let fatald ldoc = mainlogd_fatal ~code:2 id Err ldoc

  let warnd ldoc = logd Warn ldoc

  let infod ldoc = logd Info ldoc

  let debugd ldoc = logd Debug ldoc
end
