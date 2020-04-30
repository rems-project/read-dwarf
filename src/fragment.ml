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

(*****************************************************************************)
(*        At                                                                 *)
(*****************************************************************************)

let fragment_at ~env ~fenv ~size (frag : Ctype.fragment) at : Ctype.t option =
  let open Opt in
  match frag with
  | Unknown -> Ctype.machine size |> Opt.some
  | Single t -> Ctype.type_at ~env ~size t at
  | DynArray t ->
      let at = at mod Ctype.sizeof t in
      Ctype.type_at ~env ~size t at
  | FreeFragment i ->
      let frag = Env.get fenv i in
      let* (typ, off) = at_off_opt frag at in
      Ctype.type_at ~env ~size typ off

let ptr_deref ~env ~fenv ~size frag (offset : Ctype.offset) : Ctype.t option =
  match offset with
  | Const at -> fragment_at ~env ~fenv ~size frag at
  | Somewhere -> Ctype.machine size |> Opt.some

let fragment_write_at ~env ~fenv ~(ctyp : Ctype.t) (frag : Ctype.fragment) at : unit =
  match frag with
  | FreeFragment i ->
      debug "Writing at %t in %d: %t" (PP.top PP.shex at) i (PP.top Ctype.pp ctyp);
      let original = Env.get fenv i in
      let cleared = clear original ~start:at ~len:(Ctype.sizeof ctyp) in
      let newfrag = add cleared at ctyp in
      Env.set fenv i newfrag
  | _ -> ()

let ptr_write ~env ~fenv ~ctyp frag (offset : Ctype.offset) : unit =
  match offset with Const at -> fragment_write_at ~env ~fenv ~ctyp frag at | Somewhere -> ()
