(* The documentation is in the mli file *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*        Fragment                                                           *)
(*****************************************************************************)

include RngMap.Make (Ctype)

(** Add a Ctype at a specific address *)

let pp frag = frag |> bindings |> List.map (Pair.map Pp.shex Ctype.pp) |> Pp.mapping "frag"

(*****************************************************************************)
(*        Environment                                                        *)
(*****************************************************************************)

module Env = struct
  type frag = t

  type t = { frags : frag Vec.t }

  let make () =
    let frags = Vec.empty () in
    { frags }

  let copy env =
    let frags = Vec.copy env.frags in
    { frags }

  let add_typ ~addr typ env ~id =
    Vec.get env.frags id |> (fun frag -> add frag addr typ) |> Vec.set env.frags id

  let adds_frag ?(frag = empty) env = Vec.add_one env.frags frag

  let add_frag ?(frag = empty) env =
    adds_frag ~frag env;
    Vec.length env.frags - 1

  let pp fenv =
    let open Pp in
    record "state" [("frags", Vec.ppi pp fenv.frags)]

  let get t i = Vec.get t.frags i

  let set t i f = Vec.set t.frags i f
end

type env = Env.t
