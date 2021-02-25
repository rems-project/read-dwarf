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

(** This module provide a caching system for isla trace on top of {!Server}.

    It uses the cache named "isla" of {!Cache}

    Call {!start} to start and {!stop}. Do not interact directly with the {!Server}
    if you use the cache.

    You can use {!ensure_started} to force the {!Server} to start but you probably
    shouldn't do that. By the default the {!Server} is only started if the traces
    of an instruction are required and not in the cache.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** The type of Isla configuration *)
type config = Server.config

(** Implementation of {!Cache.Key} for opcodes.

   It is a special encoding of BytesSeq. If it is short enough to fit in the hash, then we do it.
    Otherwise we store in a file.

    The exact encoding is here (back mean the last/top bit of the integer, i.e. {!IntBits.back}):

    Short encoding:
      bit 0 to back -3 : The data
      bit back -3 to back: The size of the data
      bit back : cleared

    Long encoding:
      bit 0 to back -1 : The start of the data
      bit back -1: set *)
module Opcode (*: Cache.Key *) = struct
  type t = BytesSeq.t option

  let equal a b =
    match (a, b) with
    | (None, None) -> true
    | (Some bs, Some bs2) -> BytesSeq.equal bs bs2
    | _ -> false

  let hash = function
    | None -> 0
    | Some bs ->
        let i = BytesSeq.getintle_ze bs 0 in
        let l = BytesSeq.length bs in
        if l < BytesSeq.int_bytes then begin
          assert (not @@ IntBits.get i IntBits.back);
          let res = IntBits.blit l 0 i (IntBits.back - 3) 3 in
          res
        end
        else IntBits.set i IntBits.back

  let to_file file = function
    | None -> ()
    | Some bs ->
        if BytesSeq.length bs < BytesSeq.int_bytes then ()
        else
          let keyfile = Utils.Cache.to_keyfile file in
          Files.write_bin BytesSeq.output keyfile bs

  let of_file hash file =
    if hash = 0 then None
    else if IntBits.get hash IntBits.back then
      let keyfile = Utils.Cache.to_keyfile file in
      Some (Files.read BytesSeq.input keyfile)
    else
      let data = IntBits.sub hash 0 (IntBits.back - 3) in
      let size = IntBits.sub hash (IntBits.back - 3) 3 in
      let b = Bytes.create size in
      Bits.unsafe_blit_of_int data 0 b 0 (size * 8);
      Some (BytesSeq.of_bytes b)
end

(** Representation of trace lists on disk.

    It is just a list of traces separated by new lines *)
module TraceList (*: Cache.Value *) = struct
  type t = Base.rtrc list

  let to_file file (trcs : t) =
    let output_trc ochannel trc = Pp.fprintln ochannel @@ Base.pp_trc trc in
    let output_trcs = Files.output_list output_trc in
    Files.write output_trcs file trcs

  let of_file file : t =
    let num = ref 0 in
    let input_trc ichannel =
      let trc = Files.input_sexp ichannel in
      let filename = Printf.sprintf "Trace %i of %s" !num file in
      incr num;
      Base.parse_trc_string ~filename trc
    in
    let input_trcs = Files.input_list input_trc in
    Files.read input_trcs file
end

(** An epoch independant of the isla version, bump if you change the representation
    of the traces on disk.

    Reset (or not) when bumping isla version ({!Server.required_version})

    The Epoch also include the digest of the Isla configuration. Any change of configuration
    will wipe out the cache.
*)
let epoch = 4

module Epoch (*: Cache.Epoch*) = struct
  type t = string (* isla version *) * int (* epoch global var *) * string (* config digest *)

  let to_file file trcs =
    let output ochannel (s, i, d) = Printf.fprintf ochannel "%s\n%d\n%s\n" s i d in
    Files.write output file trcs

  let of_file file =
    let input ichannel =
      let version = input_line ichannel in
      let inner_epoch = int_of_string @@ input_line ichannel in
      let config =
        (* Support legacy epochs with 2 fields *)
        try input_line ichannel with End_of_file -> ""
      in
      (version, inner_epoch, config)
    in
    Files.read input file

  (** Build the epoch from the config. This function does the config [Digest] *)
  let of_config config =
    (Server.required_version, epoch, Server.Config.digest config |> Digest.to_hex)

  let compat = ( = )
end

(** The isla cache module *)
module IC = Utils.Cache.Make (Opcode) (TraceList) (Epoch)

(** This varaible stores the cache RAM representation *)
let cache : (IC.t * config) option ref = ref None

(** If this is set and cache is also set, then the server should
    be started with the architecture inside this variable when required *)
let configr : config option ref = ref None

(** Start the caching system. Do not yet start the server *)
let start (config : config) =
  configr := Some config;
  cache := Some (IC.make "isla" (Epoch.of_config config), config)

(** Stop the caching system, stop the server if it was started *)
let stop () =
  begin
    match (!cache, !configr) with
    | (Some _, None) -> Server.stop ()
    | (_, Some _) -> ()
    | (None, None) -> ()
  end;
  cache := None;
  configr := None

(** Start the server if not already started *)
let ensure_started () =
  match !configr with
  | None -> ()
  | Some arch ->
      Server.start arch;
      configr := None

(** Get the cache and fails if the cache wasn't started *)
let get_cache () =
  match !cache with Some cache -> cache | None -> failwith "Isla cache was not started"

(** Get the traces of the opcode given. Use {!Server} if the value is not in the cache *)
let get_traces (opcode : BytesSeq.t) : Base.rtrc list =
  let (cache, config) = get_cache () in
  match IC.get_opt cache (Some opcode) with
  | Some trcs -> trcs
  | None ->
      ensure_started ();
      let trcs = Server.request_bin_parsed opcode in
      let ptrcs = Preprocess.preprocess config trcs in
      IC.add cache (Some opcode) ptrcs;
      ptrcs

(** Get the traces of the nop opcode (The initialization code).
    Use {!Server} if the value is not in the cache *)
let get_nop () : Base.rtrc =
  let (cache, _) = get_cache () in
  match IC.get_opt cache None with
  | Some [trc] -> trc
  | Some _ -> fatal "Corrupted cache, nop hasn't exactly one trace"
  | None ->
      ensure_started ();
      let trcs = Server.request_bin_parsed @@ Arch.nop () in
      let trc = List.assoc true trcs in
      IC.add cache None [trc];
      trc
