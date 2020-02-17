(** This module handle architecture initialization. For now it trust default isla
    initialization but this should change soon*)

let init_state : State.t option ref = ref None

(** This function load the initial state from an initial isla trace*)
let load_init_trc (trc : State.trc) : unit =
  let _ = IslaType.type_trc trc in
  let start = State.make () in
  init_state := Some (IslaTrace.run_trc start trc)

(** Make a new initial state but add symbolically all registers not initialized by isla *)
let state () =
  match !init_state with
  | None -> failwith "Init has not been loaded"
  | Some s -> State.copy_extend s

(* TODO here I assume nop is a nop instruction in all architectures, this may not be true *)
let nop = "nop"

(** Intialize this module by calling isla on {!nop} to get initial machine state *)
let init () =
  IslaServer.(
    TEXT_ASM nop |> request |> expect_parsed_traces |> List.assoc true |> IslaManip.split_cycle
    |> fst |> IslaManip.isla_trace_conv_svar |> load_init_trc)
