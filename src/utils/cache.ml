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

(** This module implement a caching system i.e a persistant structure stored on the disk.

    A cache can be either:
     - An associative map from keys to values, See {!Make}
     - A single value, see {!Single}

    A cache must be uniquely named and will be stored in {!find_dir}[()/name]. This will
    be a directory in case of map and a file in case of a single value *)

open Fun

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Utility }

    This section is not part of the external API,
    but creating an mli file here seemed needlessly annoying.*)

(** The name of the read-dwarf cache. [.rdcache] for now. *)
let base_dir = ".rdcache"

(** Find the current cache folder as described in the README.

    When searching for a cache,
    this function will search if there already is a {!base_dir} directory either in the current
    directory or one of its parent and use the closest one it find. If it finds none and need
    a cache, it will create a new directory named {!base_dir} in the current directory.
    Therefore the returned directory always exists.
*)
let find_dir () : string =
  let rec find_folder_from cwd =
    let candidate = Filename.concat cwd base_dir in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname cwd in
      if parent = cwd then None else find_folder_from parent
  in
  let cwd = Sys.getcwd () in
  match find_folder_from cwd with
  | Some d -> d
  | None ->
      let new_cache = Filename.concat cwd base_dir in
      Unix.mkdir new_cache 0o777;
      new_cache

(** Remove a directory and all it's content.

    TODO: Make that Windows friendly

    TODO move that in {!Files}
*)
let removedir dir : unit = ignore @@ Sys.command @@ "rm -rf " ^ Filename.quote dir

(** Clear the content of directory *)
let cleardir dir =
  removedir dir;
  Unix.mkdir dir 0o777

(** Convert an hash integer to a filename: 16 characters in hexadecimal *)
let int_to_file i : string = Printf.sprintf "%016x" i

(** The type of an entry at a given hash *)
type file_type = NOPE | FILE | DIR of int

(** Give the file type of the given type. Do not count the number of entry in ther *)
let file_type s = if Sys.file_exists s then if Sys.is_directory s then DIR 0 else FILE else NOPE

(** Transform a filename in the key filename *)
let to_keyfile s = s ^ ".key"

(** [movekey old new] moves a key correponding to the given filenames, if they exist *)
let movekey oldname newname =
  let oldname = oldname ^ ".key" in
  let newname = newname ^ ".key" in
  if Sys.file_exists oldname then Sys.rename oldname newname

(** Removes a key correponding to the given filename, if it exists *)
let removekey name =
  let name = name ^ ".key" in
  if Sys.file_exists name then Sys.remove name

(** Delete the supplied cache *)
let clear_cache name =
  let dir = find_dir () in
  let cachename = Filename.concat dir name in
  if Sys.file_exists cachename then removedir cachename

(** List all existing caches *)
let list_caches () =
  let dir = find_dir () in
  let odir = Unix.opendir dir in
  let res = ref [] in
  try
    while true do
      let d = Unix.readdir odir in
      if d.[0] != '.' then res := d :: !res
    done;
    exit 42
  with End_of_file -> !res

(** Delete all the caches (and the base cache directory) *)
let clear_all_caches () =
  let dir = find_dir () in
  removedir dir

(** Thrown when adding a key that is already bound in cache *)
exception Exists

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Input signatures } *)

(** The interface of keys.

    Key are use to index values in the cache, however instead of using the key
    directly as a file name, the hexadecimal {!hash} of the key is used trough
    {!int_to_file}. See {!Make} for more details.

    There are two main way of managing keys:
    - The hash is injective, in which case there is no need for extra information.
    - The hash is not injective, in which case extra information
      required to disambiguate between different value with the
      same hash can be used. This information is stored in the file obtained with
      {!to_keyfile} file. See {!to_file}.

    An implementation can do a mix of both i.e. Putting extra information only for
    value for which there is hash collision. *)
module type Key = sig
  include Hashtbl.HashedType

  (** Write the necessary information to retrieve the key in a file.
      The filename supplied is without the key extension.
      The implementer must call {!to_keyfile} on it *)
  val to_file : string -> t -> unit

  (** Build back the key from the hash and a storage file.
      The filename supplied is without the key extension.
      The implementer must call {!to_keyfile} on it.

      If you start with a [key] and a [file], then doing
      [to_file file key]
      and then [of_file (hash key) file] must return something {!equal} to key.*)
  val of_file : int -> string -> t
end

(** An implementation of {!Key} on ints where hash is the identity and no file is written *)
module IntKey : Key with type t = int = struct
  include Stdlib.Int

  let hash i = i

  let of_file i _ = i

  let to_file _ _ = ()
end

(** The interface of values: They just need to be able to be serialized to files.
    This is isn't plain Marshal because some values may want a human readable format.*)
module type Value = sig
  type t

  (** Serialize the value to a file *)
  val to_file : string -> t -> unit

  (** Get the value back from a file. Must match with {!to_file} *)
  val of_file : string -> t
end

(** A cache can be indexed by an Epoch. When reading a cache with an incompatible epoch,
    then the cache is deleted on load. Use {!UnitEpoch} to not have this functionality.
    {!Single} caches currently do not support epochs.*)
module type Epoch = sig
  include Value

  (** Tests if epochs are compatible *)
  val compat : t -> t -> bool
end

(** A dummy epochs implementation, if not epoch is needed *)
module UnitEpoch = struct
  type t = unit

  let to_file _ () = ()

  let of_file _ = ()

  let compat () () = true
end

module IntEpoch = struct
  type t = int

  let to_file file i = Files.write_bin output_binary_int file i

  let of_file file = Files.read_bin input_binary_int file

  let compat = ( = )
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 CacheMap } *)

(** The output signature of {!Make} *)
module type S = sig
  (** The type of keys *)
  type key

  (** The type of values *)
  type value

  (** The type of the epoch *)
  type epoch

  (** The type that represent the cache in RAM. *)
  type t

  (** Build a new cache management object with a name and an epoch
      If [fake] is set, the cache will not touch the disk and behave as a plain Hashtbl.*)
  val make : ?fake:bool -> string -> epoch -> t

  (** Get a value from the cache or [None] if no value is bound to the key *)
  val get_opt : t -> key -> value option

  (** Get a value from the cache or throws [Not_found] if no value is bound *)
  val get : t -> key -> value

  (** Add a new binding. Fails if a binding already exists *)
  val add : t -> key -> value -> unit

  (** Remove a binding (Also delete the representation on disk *)
  val remove : t -> key -> unit
end

(** The Key must provide the {!Key} interface,
    where the values must satisfy the {!Value} interface.

    The map is stored on the disk in a simple way. First the key is hashed and this
    hashed gives a filename with {!int_to_file}. If there is no collision, then the
    value is stored in that file and the key is stored (optionally) in [hash.key]
    file (See {!Key} for details).

    If there is collision then the filename is a directory containing numbered files
    of all the key-value pairs. Again the value is in file named [n] when the key is
    (optionally) in file named [n.key].
*)
module Make (Key : Key) (Value : Value) (Epoch : Epoch) :
  S with type key = Key.t and type value = Value.t and type epoch = Epoch.t = struct
  (* TODO: If memory footprint is too big, try not to keep everything in RAM *)
  module RAMMap = Hashtbl.Make (Key)

  type key = Key.t

  type value = Value.t

  type epoch = Epoch.t

  type t = {
    name : string;
    epoch : epoch;
    dir : string;
    loaded : (int, file_type) Hashtbl.t;
    rammap : value RAMMap.t;
    fake : bool;
  }

  let make ?(fake = false) name epoch =
    let loaded = Hashtbl.create 100 in
    let rammap = RAMMap.create 100 in
    if fake then begin
      info "Starting fake cache %s" name;
      { name; epoch; dir = ""; rammap; loaded; fake }
    end
    else
      let dir = Filename.concat $ find_dir () $ name in
      info "Starting cache %s in %s" name dir;
      if not @@ Sys.file_exists dir then Unix.mkdir dir 0o777;
      let epoch_file = Filename.concat dir "epoch" in
      if Sys.file_exists epoch_file then begin
        info "Checking epoch of %s cache" name;
        let e = Epoch.of_file epoch_file in
        if Epoch.compat e epoch then info "Epoch of %s is compatible" name
        else begin
          info "Epoch of %s is incompatible, clearing %s" name dir;
          cleardir dir;
          Epoch.to_file epoch_file epoch
        end
      end
      else Epoch.to_file epoch_file epoch;
      { name; epoch; dir; rammap; loaded; fake }

  (** Loads the value of key into the hashmap *)
  let load map hash =
    if not @@ Hashtbl.mem map.loaded hash then
      let filename = Filename.concat map.dir @@ int_to_file hash in
      let load_file filename =
        let key = Key.of_file hash filename in
        let value = Value.of_file filename in
        RAMMap.add map.rammap key value
      in
      match file_type filename with
      | NOPE -> Hashtbl.add map.loaded hash NOPE
      | FILE ->
          load_file filename;
          Hashtbl.add map.loaded hash FILE
      | DIR _ ->
          let rec load (i : int) =
            let filenamei = Filename.concat filename (int_to_file i) in
            if Sys.file_exists filenamei then begin
              load_file filenamei;
              load (i + 1)
            end
            else Hashtbl.add map.loaded hash (DIR i)
          in
          load 0

  let ensure_load map key =
    if map.fake then ()
    else
      let hash = Key.hash key in
      load map hash

  let get_opt map key : value option =
    ensure_load map key;
    RAMMap.find_opt map.rammap key

  let get map key = match get_opt map key with Some v -> v | None -> raise Not_found

  let add map key value =
    ensure_load map key;
    if RAMMap.mem map.rammap key then raise Exists;
    RAMMap.add map.rammap key value;
    if not map.fake then
      let hash = Key.hash key in
      let filename = Filename.concat map.dir @@ int_to_file hash in
      match Hashtbl.find map.loaded hash with
      | NOPE ->
          Key.to_file filename key;
          Value.to_file filename value;
          Hashtbl.replace map.loaded hash FILE
      | FILE ->
          let tmpname = filename ^ ".tmp" in
          let newname = Filename.concat filename (int_to_file 0) in
          Sys.rename filename tmpname;
          Unix.mkdir filename 0o777;
          Sys.rename tmpname newname;
          movekey filename newname;
          let filename1 = Filename.concat filename (int_to_file 1) in
          Key.to_file filename1 key;
          Value.to_file filename1 value;
          Hashtbl.replace map.loaded hash (DIR 2)
      | DIR n ->
          let filenamen = Filename.concat filename (int_to_file n) in
          Key.to_file filenamen key;
          Value.to_file filenamen value;
          Hashtbl.replace map.loaded hash (DIR (n + 1))

  let remove map key =
    ensure_load map key;
    if RAMMap.mem map.rammap key then begin
      RAMMap.remove map.rammap key;
      if not map.fake then
        let hash = Key.hash key in
        let filename = Filename.concat map.dir @@ int_to_file hash in
        match Hashtbl.find map.loaded hash with
        | NOPE -> assert false
        | FILE ->
            Sys.remove filename;
            removekey filename;
            Hashtbl.replace map.loaded hash NOPE
        | DIR n ->
            let rec move_back n =
              let filenamen = Filename.concat filename (int_to_file n) in
              if Sys.file_exists filenamen then begin
                let filenamenm1 = Filename.concat filename (int_to_file (n - 1)) in
                Sys.rename filenamen filenamenm1;
                movekey filenamen filenamenm1;
                move_back (n + 1)
              end
              else ()
            in
            let rec check n =
              let filenamen = Filename.concat filename (int_to_file n) in
              (* if assert fails, some external agent fucked with the cache during execution *)
              assert (Sys.file_exists filenamen);
              let ckey = Key.of_file hash filenamen in
              if Key.equal key ckey then begin
                Sys.remove filenamen;
                removekey filenamen;
                move_back (n + 1)
              end
              else check (n + 1)
            in
            check 0;
            if n > 1 then Hashtbl.replace map.loaded hash (DIR (n - 1))
            else begin
              Sys.remove filename;
              Hashtbl.replace map.loaded hash NOPE
            end
    end
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Single Cache } *)

(** The signature of the output of {!Single} *)
module type SingleS = sig
  (** The type of the stored value *)
  type value

  (** The type that represent the cache in RAM. *)
  type t

  (** Build a new cache management object with a name
      If [fake] is set, the cache will not touch the disk and behave as a plain ref.*)
  val make : ?fake:bool -> string -> t

  (** Get a value from the cache or [None] if no value is stored *)
  val get_opt : t -> value option

  (** Get a value from the cache or throws [Not_found] if no value is bound *)
  val get : t -> value

  (** Set the value in the cache *)
  val set : t -> value -> unit

  (** Clear the value in the cache *)
  val clear : t -> unit
end

(** The functor is to make a single cached value. This do not support epochs (Yet)

    TODO: Maybe the code would be simpler if this was a map from unit to the value.
*)
module Single (Value : Value) : SingleS with type value = Value.t = struct
  type value = Value.t

  type t = { name : string; file : string; mutable value : value option; fake : bool }

  let make ?(fake = false) name =
    if fake then { name; file = ""; value = None; fake }
    else
      let file = Filename.concat $ find_dir () $ name in
      if not @@ Sys.file_exists file then { name; file; value = None; fake }
      else
        let value = Value.of_file file in
        { name; file; value = Some value; fake }

  let get_opt s = s.value

  let get s = match get_opt s with Some v -> v | None -> raise Not_found

  let set s value =
    s.value <- Some value;
    if not s.fake then Value.to_file s.file value

  let clear s =
    s.value <- None;
    if not s.fake then if Sys.file_exists s.file then Sys.remove s.file
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Command line operations } *)

(** The cache command line. TODO documentation of the inside *)
module Cmd = struct
  open Cmdliner

  (** This module provide a int -> string cache for testing pruposes.
      It can be tested with [read-dwarf cache --test] *)
  module Test = struct
    module Key : Key with type t = int = struct
      type t = int

      (* Artificially create collisions for testing purposes *)
      let hash i = i mod 10

      let equal i j = i = j

      let to_file filename i =
        let keyfile = to_keyfile filename in
        let content = string_of_int (i / 10) in
        Files.write_string keyfile content

      let of_file hash filename =
        let keyfile = to_keyfile filename in
        let content = Files.read_string keyfile in
        (10 * int_of_string content) + hash
    end

    module Value : Value with type t = string = struct
      type t = string

      let to_file = Files.write_string

      let of_file = Files.read_string
    end

    module Cache = Make (Key) (Value) (UnitEpoch)
    module Single = Single (Value)
  end

  let clear =
    let doc = "Clear the cache named (or all if -a/--all is passed)" in
    Arg.(value & flag & info ["c"; "clear"] ~doc)

  let all =
    let doc = "When used, do the operation on all the caches" in
    Arg.(value & flag & info ["a"; "all"] ~doc)

  let test =
    let doc = "Pops mini CLI to test the cache system on a cache named test." in
    Arg.(value & flag & info ["t"; "test"] ~doc)

  let list =
    let doc = "List the caches" in
    Arg.(value & flag & info ["l"; "list"] ~doc)

  let arg =
    let doc = "Cache on which to do the operation" in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"CACHE_NAME" ~doc)

  let fake =
    let doc = "With --test: Fake the caching and do not write or read anything from disk" in
    Arg.(value & flag & info ["f"; "fake"] ~doc)

  (** The cache operataion to run *)
  type operation = CLEAR | LIST | TEST

  (** Input flags to mode conversion *)
  let op_f2m clear list test : operation Term.ret =
    match (clear, list, test) with
    | (false, false, false) ->
        `Error (false, "You must specific an operation like --clear,--list or --test")
    | (false, true, false) -> `Ok LIST
    | (true, false, false) -> `Ok CLEAR
    | (false, false, true) -> `Ok TEST
    | _ -> `Error (false, "You cannot specify multiple operations")

  let operation_term = Term.(ret (const op_f2m $ clear $ list $ test))

  (** The testing mini command line to test the {!Test} cache *)
  let test fake =
    let c = Test.Cache.make ~fake "test" () in
    let sing = Test.Single.make ~fake "testsingle" in
    let print_help () =
      print_endline "Cache testing help:";
      print_endline "  help         - Print this";
      print_endline "  get num      - Get value stored at num";
      print_endline "  add num text - Store text at num";
      print_endline "  rem num      - Remove the text stored at num";
      print_endline "  gets         - Get the value of the single cache";
      print_endline "  set text     - Set the single cache to text";
      print_endline "  clr          - Clear the single cache";
      print_newline ()
    in
    print_help ();
    try
      while true do
        print_string "> ";
        let s = read_line () |> String.split_on_char ' ' in
        match s with
        | "help" :: _ -> print_help ()
        | ["get"; key] -> Pp.(println $ opt string (Test.Cache.get_opt c (int_of_string key)))
        | ["add"; num; text] -> Test.Cache.add c (int_of_string num) text
        | ["rem"; num] -> Test.Cache.remove c (int_of_string num)
        | ["gets"] -> Pp.(println $ opt string (Test.Single.get_opt sing))
        | ["set"; text] -> Test.Single.set sing text
        | ["clr"] -> Test.Single.clear sing
        | _ ->
            print_endline "Error: type \"help\" for help";
            print_string "x "
      done
    with End_of_file ->
      print_newline ();
      print_endline "Goodbye and good luck on your future testing adventures!"

  (** Do the caching operation [op] *)
  let dostuff op all arg fake =
    match (op, arg) with
    | (CLEAR, Some c) -> if all then clear_all_caches () else clear_cache c
    | (CLEAR, None) ->
        if all then clear_all_caches () else fatal " You must specify a cache to delete or --all"
    | (LIST, _) ->
        let l = list_caches () in
        Pp.(println $ separate hardline (List.map string l))
    | (TEST, _) -> test fake

  open CmdlinerHelper

  let term = Term.(func_options [Logs.term] dostuff $ operation_term $ all $ arg $ fake)

  let exits =
    let doc = "on external errors, (Parsing error, Typing error, ...)." in
    let doc2 = "on non-exception internal errors like assertion failed." in
    Cmd.Exit.(info ~doc 1 :: info ~doc:doc2 2 :: defaults)

  let info =
    let doc = "Manipulate the caching system" in
    Cmd.(info "cache" ~doc ~exits)

  let command = (term, info)
end
