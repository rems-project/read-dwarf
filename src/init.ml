(** This module handle architecture initialization. For now it trust default isla
    initialization but this should change soon (TODO)*)

(** The initial state *)
let init_state : State.t option ref = ref None

(** Intialize this module by calling Isla on {!Arch.nop} to get initial machine state *)
let init () =
  let trc =
    IslaCache.get_nop () |> IslaManip.split_cycle |> fst
    |> IslaManip.remove_ignored (Arch.get_isla_config ()).ignored_regs
  in
  IslaType.type_trc trc |> ignore;
  let state = State.make () in
  (IslaRun.trc_mut [@ocaml.warning "-3"] (* deprecated *)) state trc;
  State.lock state;
  init_state := Some state;
  state

(** Return the initial state. Compute it if required. *)
let state () = match !init_state with None -> init () | Some s -> s
