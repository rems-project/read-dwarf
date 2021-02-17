(** This module handle architecture initialization. For now it trust default isla
    initialization but this should change soon (TODO)*)

(** The initial state *)
let init_state : State.t option ref = ref None

(** Intialize this module by calling Isla on {!Arch.nop} to get initial machine state *)
let init () =
  let trc =
    Isla.Cache.get_nop () |> Isla.Manip.split_cycle |> fst
    |> Isla.Manip.remove_ignored (Arch.get_isla_config ()).ignored_regs
  in
  Isla.Type.type_trc trc |> ignore;
  let state = State.make () in
  (Isla.Run.trc_mut [@ocaml.warning "-3"] (* deprecated *)) state trc;
  State.lock state;
  init_state := Some state;
  state

(** Return the initial state. Compute it if required. *)
let state () = match !init_state with None -> init () | Some s -> s
