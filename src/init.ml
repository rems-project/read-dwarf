(** This module handle architecture initialization. For now it trust default isla
    initialization but this should change soon*)

let init_state : State.t option ref = ref None

(** This function load the initial state from an initial isla trace*)
let load_init_trc (trc : Isla.rtrc) : unit =
  let _ = IslaType.type_trc trc in
  let state = State.make () in
  IslaRun.trc_mut state trc;
  State.lock state;
  init_state := Some state

(** Make a new initial state but add symbolically all registers not initialized by isla *)
let state () =
  match !init_state with
  | None -> failwith "Init has not been loaded"
  | Some s ->
      State.unsafe_unlock s;
      State.extend_mut s;
      State.lock s;
      s

(** Intialize this module by calling isla on {!nop} to get initial machine state *)
let init () =
  IslaCache.get_nop () |> IslaManip.split_cycle |> fst
  |> IslaManip.remove_ignored (Arch.get_isla_config ()).ignored_regs
  |> load_init_trc
