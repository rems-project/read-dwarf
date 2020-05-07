(* The documentation is in the mli file *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*        Fragment                                                           *)
(*****************************************************************************)

include RngMap.Make (Ctype)

(** Add a Ctype at a specific address *)

let pp frag = frag |> bindings |> List.map (Pair.map PP.shex Ctype.pp) |> PP.mapping "frag"

(*****************************************************************************)
(*        Environment                                                        *)
(*****************************************************************************)

module Env = struct
  type frag = t

  type t = { frags : frag Vector.t }

  let make () =
    let frags = Vector.empty () in
    { frags }

  let copy env =
    let frags = Vector.copy env.frags in
    { frags }

  let add_typ ~tenv ~addr typ env ~id =
    Vector.get env.frags id |> (fun frag -> add frag addr typ) |> Vector.set env.frags id

  let adds_frag ?(frag = empty) env = Vector.add_one env.frags frag

  let add_frag ?(frag = empty) env =
    adds_frag ~frag env;
    Vector.length env.frags - 1

  let pp fenv =
    let open PP in
    record "state" [("frags", Vector.ppi pp fenv.frags)]

  let get t i = Vector.get t.frags i

  let set t i f = Vector.set t.frags i f
end

type env = Env.t
