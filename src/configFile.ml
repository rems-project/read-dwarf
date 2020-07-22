open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Toml helpers } *)

module Table = TomlTypes.Table

type table = TomlTypes.table

type value = TomlTypes.value

module Key = Table.Key

type key = Key.t

exception InvalidKey of string * string list

(* Expectors *)

let toml_expect_bool name : value -> bool = function
  | TBool b -> b
  | _ -> raise (InvalidKey ("bad type, expected boolean", [name]))

let toml_expect_string name : value -> string = function
  | TString s -> s
  | _ -> raise (InvalidKey ("bad type, expected string", [name]))
  (* This function is unused, but it may be needed by future configuration options *)
  [@@ocaml.warning "-32"]

let toml_expect_int name : value -> int = function
  | TInt i -> i
  | _ -> raise (InvalidKey ("bad type, expected string", [name]))
  (* This function is unused, but it may be needed by future configuration options *)
  [@@ocaml.warning "-32"]

(* Getters *)

let toml_get_string (table : table) (key : key) : string =
  match Table.find_opt key table with
  | None -> raise (InvalidKey ("missing key", [Key.to_string key]))
  | Some (TString s) -> s
  | Some _ -> raise (InvalidKey ("bad type, expected string", [Key.to_string key]))

let toml_get_by_string_opt (func : string -> 'a) (table : table) (key : key) : 'a option =
  match Table.find_opt key table with
  | None -> None
  | Some (TString s) ->
      let a =
        try func s with e -> raise (InvalidKey (Printexc.to_string e, [Key.to_string key]))
      in
      Some a
  | Some _ -> raise (InvalidKey ("bad type, expected string or nothing", [Key.to_string key]))

let toml_get_by_int_opt (func : int -> 'a) (table : table) (key : key) : 'a option =
  match Table.find_opt key table with
  | None -> None
  | Some (TInt i) ->
      let i =
        try func i with e -> raise (InvalidKey (Printexc.to_string e, [Key.to_string key]))
      in
      Some i
  | Some _ -> raise (InvalidKey ("bad type, expected integer or nothing", [Key.to_string key]))

let toml_get_int_opt = toml_get_by_int_opt Fun.id

let toml_get_string_list (table : table) (key : key) : string list =
  match Table.find_opt key table with
  | None -> raise (InvalidKey ("missing key", [Key.to_string key]))
  | Some (TArray (NodeString sl)) -> sl
  | Some (TArray NodeEmpty) -> []
  | Some _ -> raise (InvalidKey ("bad type, expected string list", [Key.to_string key]))

let toml_get_file ~path (table : table) (key : key) : string =
  match Table.find_opt key table with
  | None -> raise (InvalidKey ("missing key", [Key.to_string key]))
  | Some (TString s) ->
      let res = Files.add_to_relative ~newp:path s in
      if Files.exists res then res
      else raise (InvalidKey ("file do not exist", [Key.to_string key]))
  | Some _ -> raise (InvalidKey ("bad type, expected filename", [Key.to_string key]))

let toml_get_table (func : table -> 'a) (table : table) (key : key) : 'a =
  match Table.find_opt key table with
  | None -> raise (InvalidKey ("missing table", [Key.to_string key]))
  | Some (TTable t) -> (
      try func t with InvalidKey (s, sl) -> raise (InvalidKey (s, Key.to_string key :: sl))
    )
  | Some _ -> raise (InvalidKey ("bad type, expected table", [Key.to_string key]))

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Configuration structure } *)

module ArchConf = struct
  module Isla = struct
    (* TODO At some point it would be good to integrate this with native isla config files *)

    type t = {
      ignored_regs : string list;
      arch_file : string;
      arch_file_digest : string;
      linearize : string list;
      config_registers : (string * bool) list;
      other_opts : string list;
    }

    let digest t =
      let b = Buffer.create 100 in
      let add_with_space s =
        Buffer.add_string b s;
        Buffer.add_char b ' '
      in
      let add_with_bool (s, bl) =
        Buffer.add_string b s;
        Buffer.add_char b ' ';
        if bl then Buffer.add_string b "true" else Buffer.add_string b "false";
        Buffer.add_char b ' '
      in
      List.iter add_with_space t.ignored_regs;
      Buffer.add_char b '\n';
      Buffer.add_string b (Digest.to_hex t.arch_file_digest);
      Buffer.add_char b '\n';
      List.iter add_with_space t.linearize;
      Buffer.add_char b '\n';
      List.iter add_with_bool t.config_registers;
      Buffer.add_char b '\n';
      List.iter add_with_space t.other_opts;
      Buffer.add_char b '\n';
      Digest.string (Buffer.contents b)

    let ignored_regs_key = Toml.key "ignored-regs"

    let arch_file_key = Toml.key "arch-file"

    let linearize_key = Toml.key "linearize"

    let config_registers_key = Toml.key "config-registers"

    let other_opts_key = Toml.key "other-opts"

    let of_toml ~path table =
      let ignored_regs = toml_get_string_list table ignored_regs_key in
      let linearize = toml_get_string_list table linearize_key in
      let other_opts = toml_get_string_list table other_opts_key in
      let arch_file = toml_get_file ~path table arch_file_key in
      let config_registers_of_toml table =
        table |> Table.to_seq
        |> Seq.map (fun (s, b) ->
               let s = Key.to_string s in
               (s, toml_expect_bool s b))
        |> List.of_seq
      in
      let config_registers = toml_get_table config_registers_of_toml table config_registers_key in
      let arch_file_digest = Digest.file arch_file in
      { ignored_regs; linearize; other_opts; arch_file; arch_file_digest; config_registers }

    let pp_raw ic =
      let open PP in
      record "isla-config"
        [
          ("ignored-regs", list string ic.ignored_regs);
          ("arch-file", string ic.arch_file);
          ("arch-file digest (MD5)", string (Digest.to_hex ic.arch_file_digest));
          ("linearize", list string ic.linearize);
          ("config_registers", list (pair string bool) ic.config_registers);
          ("other-opts", list string ic.other_opts);
        ]
  end

  type t = { arch : ArchType.t; toolchain : string; isla : Isla.t }

  let isla_key = Toml.key "isla"

  let toolchain_key = Toml.key "toolchain"

  let of_toml ~arch ~path table =
    let toolchain = toml_get_string table toolchain_key in
    let isla = toml_get_table (Isla.of_toml ~path) table isla_key in
    { arch; toolchain; isla }

  let pp_raw aconf =
    let open PP in
    record "arch-config"
      [
        ("arch", ArchType.pp aconf.arch);
        ("toolchain", string aconf.toolchain);
        ("isla", Isla.pp_raw aconf.isla);
      ]
end

module Z3 = struct
  type t = {
    timeout : int option;  (** Timeout for individual requests in milliseconds *)
    memory : int option;  (** Maximum memory, in MegaBytes *)
    simplify_opts : (string * bool) list;  (** List of option for the simplify command *)
  }

  let timeout_key = Toml.key "timeout"

  let memory_key = Toml.key "memory"

  let simplify_opts_key = Toml.key "simplify-opts"

  let of_toml table =
    let timeout = toml_get_int_opt table timeout_key in
    let memory = toml_get_int_opt table memory_key in
    let simplify_opts_of_toml table =
      table |> Table.to_seq
      |> Seq.map (fun (s, b) ->
             let s = Key.to_string s in
             (s, toml_expect_bool s b))
      |> List.of_seq
    in
    let simplify_opts = toml_get_table simplify_opts_of_toml table simplify_opts_key in
    { timeout; memory; simplify_opts }

  let pp_raw z3c =
    let open PP in
    record "z3-config"
      [
        ("timeout", opt int z3c.timeout);
        ("memory", opt int z3c.memory);
        ("simplify-opts", list (pair string bool) z3c.simplify_opts);
      ]
end

let arch_key = Toml.key "default-arch"

let archs_key = Toml.key "archs"

let z3_key = Toml.key "z3"

type archs_type = (ArchType.t, ArchConf.t) Hashtbl.t

let archs_of_toml ~path (table : table) : archs_type =
  try
    Table.to_seq table
    |> Seq.map (fun (k, (v : value)) ->
           let name = Key.to_string k in
           match v with
           | TTable t -> (
               try
                 let arch = ArchType.of_string name in
                 (arch, ArchConf.of_toml ~arch ~path t)
               with InvalidKey (s, kl) -> raise (InvalidKey (s, Key.to_string k :: kl))
             )
           | _ -> raise (InvalidKey ("bad type, expected table", [Key.to_string k])))
    |> Hashtbl.of_seq
  with Failure s | Invalid_argument s -> raise (InvalidKey (s, []))

type t = { arch : ArchType.t option; archs : archs_type; z3 : Z3.t }

let of_toml ~path table =
  let arch = toml_get_by_string_opt ArchType.of_string table arch_key in
  let archs = toml_get_table (archs_of_toml ~path) table archs_key in
  let z3 = toml_get_table Z3.of_toml table z3_key in
  { arch; archs; z3 }

let pp_raw conf =
  let open PP in
  record "config"
    [
      ("default-arch", opt ArchType.pp conf.arch);
      ("archs", hashtbl ArchType.pp ArchConf.pp_raw conf.archs);
      ("z3", Z3.pp_raw conf.z3);
    ]

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Top level interaction } *)

let of_file file : t =
  let path = Filename.dirname file in
  let table =
    match Toml.Parser.from_filename file with
    | `Ok table -> table
    | `Error (message, location) ->
        fatal "File \"%s\", line %d, character %d: TOML config parse error: %s" location.source
          location.line location.column message
  in
  try
    let conf = of_toml ~path table in
    debug "loaded config:\n%t" (PP.topi pp_raw conf);
    conf
  with InvalidKey (message, path) ->
    fatal "TOML config error for option %s: %s" (String.concat "." path) message

let data : t option ref = ref None

let load file =
  assert (!data = None);
  data := Some (of_file file)

let ensure_loaded file = match !data with Some _ -> () | None -> load file

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Accessors } *)

exception UnloadedConfig

let raise_unloaded _ = raise UnloadedConfig

let get_config () = Opt.value_fun !data ~default:raise_unloaded

let default_arch =
  try ArchType.of_string ConfigPre.default_arch
  with Invalid_argument s -> fatal "in default_arch in config.ml: %s" s

let get_arch_name () = match (get_config ()).arch with Some a -> a | None -> default_arch

let get_arch_config arch = Hashtbl.find (get_config ()).archs arch

let get_isla_config arch = (get_arch_config arch).isla

let get_z3_config () = (get_config ()).z3
