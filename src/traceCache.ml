(** This module provides a caching system for fully processed traces *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Fun
module Opcode = IslaCache.Opcode
module Epoch = IslaCache.Epoch

module Traces = struct
  open Trace

  type t = Trace.t list

  type regs = (Reg.path, string list * Reg.ty) Hashtbl.t

  let to_file file (traces : t) =
    let writer channel traces =
      let regs : regs = Hashtbl.create 5 in
      let add_reg reg =
        Hashtbl.add regs reg (Reg.path_to_string_list reg, Reg.path_type reg |> Reg.expect_plain)
      in
      let add_regs_exp =
        AstManip.exp_iter_var (function Var.Register reg -> add_reg reg | _ -> ())
      in
      let add_regs = function
        | WriteReg { reg; value } ->
            add_reg reg;
            add_regs_exp value
        | ReadMem { addr; _ } -> add_regs_exp addr
        | WriteMem { addr; value; _ } ->
            add_regs_exp addr;
            add_regs_exp value
        | Assert exp -> add_regs_exp exp
      in
      List.iter (List.iter add_regs) traces;
      Marshal.to_channel channel regs [Marshal.No_sharing];
      Marshal.to_channel channel traces [Marshal.No_sharing]
    in
    Files.write_bin writer file traces

  let of_file file : t =
    let reader file =
      let new_regs = Hashtbl.create 10 in
      let regs : regs = Marshal.from_channel file in
      Hashtbl.iter
        (fun old_path (sl, ty) ->
          debug "Adding %t with type %t" PP.(top (list string) sl) PP.(top Reg.pp_ty ty);
          Reg.add_path sl ty;
          Hashtbl.add new_regs old_path (Reg.path_of_string_list sl))
        regs;
      let old_traces : t = Marshal.from_channel file in
      let update_reg = Hashtbl.find new_regs in
      let update_exp =
        AstManip.exp_map_var (function
          | Var.Register reg -> Var.Register (update_reg reg)
          | v -> v)
      in
      let update_event = function
        | WriteReg { reg; value } -> WriteReg { reg = update_reg reg; value = update_exp value }
        | ReadMem rm -> ReadMem { rm with addr = update_exp rm.addr }
        | WriteMem wm ->
            WriteMem { wm with addr = update_exp wm.addr; value = update_exp wm.value }
        | Assert exp -> Assert (update_exp exp)
      in
      let traces = List.map (List.map update_event) old_traces in
      traces
    in
    Files.read_bin reader file
end

module TC = Cache.Make (Opcode) (Traces) (Epoch)

let cache : TC.t option ref = ref None

type config = IslaCache.config

(** Start the caching system. Start {!IslaCache} too *)
let start (config : IslaCache.config) =
  IslaCache.start config;
  cache := Some (TC.make "traces" (Epoch.of_config config))

(** Stop the caching system *)
let stop () =
  begin
    match !cache with
    | Some _ -> IslaCache.stop ()
    | None -> ()
  end;
  cache := None

(** Get the cache and fails if the cache wasn't started *)
let get_cache () =
  match !cache with Some cache -> cache | None -> failwith "Trace cache was not started"

(** Get the traces of the opcode given. Use {!IslaServer} if the value is not in the cache *)
let get_traces (opcode : BytesSeq.t) : Trace.t list =
  let cache = get_cache () in
  match TC.get_opt cache (Some opcode) with
  | Some trcs -> trcs
  | None ->
      let isla_traces = IslaCache.get_traces opcode in
      let traces = List.map (tee (IslaType.type_trc %> ignore) %> Trace.of_isla) isla_traces in
      let straces = List.map Trace.simplify traces in
      TC.add cache (Some opcode) straces;
      straces
