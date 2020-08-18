(** This module provide a caching system for isla trace on top of {!IslaServer}.

    It uses the cache named "isla" of {!Cache}

    Call {!start} to start and {!stop}. Do not interact directly with the {!IslaServer}
    if you use the cache.

    You can use {!ensure_started} to force the {!IslaServer} to start but you probably
    shouldn't do that. By the default the {!IslaServer} is only started if the traces
    of an instruction are required and not in the cache.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** The type of Isla configuration *)
type config = IslaServer.config

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
          let keyfile = Cache.to_keyfile file in
          Files.write_bin BytesSeq.output keyfile bs

  let of_file hash file =
    if hash = 0 then None
    else if IntBits.get hash IntBits.back then
      let keyfile = Cache.to_keyfile file in
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
  type t = Isla.rtrc list

  let to_file file (trcs : t) =
    let output_trc ochannel trc = PP.fprintln ochannel @@ Isla.pp_trc trc in
    let output_trcs = Files.output_list output_trc in
    Files.write output_trcs file trcs

  let of_file file : t =
    let num = ref 0 in
    let input_trc ichannel =
      let trc = Files.input_sexp ichannel in
      let filename = Printf.sprintf "Trace %i of %s" !num file in
      incr num;
      Isla.parse_trc_string ~filename trc
    in
    let input_trcs = Files.input_list input_trc in
    Files.read input_trcs file
end

(** An epoch independant of the isla version, bump if you change the representation
    of the traces on disk.

    Reset (or not) when bumping isla version ({!IslaServer.required_version})

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
    (IslaServer.required_version, epoch, IslaServer.Config.digest config |> Digest.to_hex)

  let compat = ( = )
end

(** The isla cache module *)
module IC = Cache.Make (Opcode) (TraceList) (Epoch)

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
    | (Some _, None) -> IslaServer.stop ()
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
      IslaServer.start arch;
      configr := None

(** Get the cache and fails if the cache wasn't started *)
let get_cache () =
  match !cache with Some cache -> cache | None -> failwith "Isla cache was not started"

(** Get the traces of the opcode given. Use {!IslaServer} if the value is not in the cache *)
let get_traces (opcode : BytesSeq.t) : Isla.rtrc list =
  let (cache, config) = get_cache () in
  match IC.get_opt cache (Some opcode) with
  | Some trcs -> trcs
  | None ->
      ensure_started ();
      let trcs = IslaServer.request_bin_parsed opcode in
      let ptrcs = IslaPreprocess.preprocess config trcs in
      IC.add cache (Some opcode) ptrcs;
      ptrcs

(** Get the traces of the nop opcode (The initialization code).
    Use {!IslaServer} if the value is not in the cache *)
let get_nop () : Isla.rtrc =
  let (cache, _) = get_cache () in
  match IC.get_opt cache None with
  | Some [trc] -> trc
  | Some _ -> fatal "Corrupted cache, nop hasn't exactly one trace"
  | None ->
      ensure_started ();
      let trcs = IslaServer.request_bin_parsed @@ Arch.nop () in
      let trc = List.assoc true trcs in
      IC.add cache None [trc];
      trc
