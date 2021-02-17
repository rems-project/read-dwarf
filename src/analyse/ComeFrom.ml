(*****************************************************************************)
(**  invert control-flow data to get come-from data                          *)

(*****************************************************************************)

open Utils
open ControlFlowTypes
open ControlFlow

let mk_come_froms instructions : come_from list array =
  let size = Array.length instructions in
  let come_froms = Array.make size [] in
  Array.iteri
    (function
      | k -> (
          function
          | i ->
              List.iter
                (function
                  | (tk, _, k', s) ->
                      let come_from =
                        {
                          cf_target_kind = tk;
                          cf_addr = i.i_addr;
                          cf_index = k;
                          cf_control_flow = i.i_control_flow;
                          cf_desc = s;
                        }
                      in
                      if k' < size then come_froms.(k') <- come_from :: come_froms.(k') else ())
                i.i_targets
        ))
    instructions;
  Array.iteri
    (function
      | k -> (
          function cfs -> come_froms.(k) <- List.rev cfs
        ))
    come_froms;
  come_froms

let pp_come_froms (addr : addr) (cfs : come_from list) : string =
  match cfs with
  | [] -> ""
  | _ ->
      " <- "
      ^ String.concat ","
          (List.map
             (function
               | cf ->
                   pp_come_from_addr_wrt addr cf.cf_control_flow cf.cf_addr
                   ^ "("
                   ^ pp_target_kind_short cf.cf_target_kind
                   (*^ pp_control_flow_instruction_short c*)
                   ^ ")"
                   ^ cf.cf_desc)
             cfs)
