(** The module provide the concept of stateDiffs: a diff between two states

    In all diff-like functions, the old version is the first argument and the new version
    is the second argument like CLI [diff]
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Fun

type state = State.t

type tval = State.tval

type ty = State.ty

type exp = State.exp

(** Make a diff between two list but with only addition.

    It is expected that the old list is sublist of the new list, otherwise a warning is raised *)
let rec list_diff eq name list1 list2 =
  match list1 with
  | [] -> list2
  | head :: _ -> (
      match list2 with
      | [] ->
          warn "stateDiff: while diffing list about %s, reached empty list" name;
          []
      | a :: _ as all when eq a head && List.equal eq all list1 -> []
      | a :: l -> a :: list_diff eq name list1 l
    )

(** This module provide diff utility between {!State.Mem.t} values *)
module Mem = struct
  type t = { trace_diff : State.Mem.event list }

  let diff (m1 : State.Mem.t) (m2 : State.Mem.t) =
    { trace_diff = list_diff State.Mem.equal_event "memory" m1.trace m2.trace }

  let pp (md : t) = PP.(list State.Mem.pp_event (List.rev md.trace_diff))
end

(** This module provide diff utility between fragment environment ({!Fragment.Env.t}) *)

(* module Fenv = struct
 *   type modif_elem = Add of int * Ctype.t | Rem of int | Mod of int * Ctype.t
 *
 *   type modif = modif_elem list
 *
 *   type t = { modifs : (int * modif) list; news : (int * Fragment.t) list }
 *
 *   (\** Make a diff between fragment *\)
 *   let frag_diff (f1 : Fragment.t) (f2 : Fragment.t) : modif_elem list =
 *     let l1 = Fragment.bindings f1 in
 *     let l2 = Fragment.bindings f2 in
 *     let rec diff l1 l2 =
 *       match (l1, l2) with
 *       | ([], _) -> List.map (fun (i, c) -> Add (i, c)) l1
 *       | (_, []) -> List.map (fun (i, c) -> Rem i) l1
 *       | ((a1, ct1) :: t1, (a2, ct2) :: t2) when a1 = a2 && Ctype.equal ct1 ct2 -> diff t1 t2
 *       | ((a1, ct1) :: t1, (a2, ct2) :: t2) when a1 = a2 -> Mod (a2, ct2) :: diff t1 t2
 *       | ((a1, ct1) :: t1, (a2, ct2) :: t2) when a1 < a2 -> Rem a1 :: diff t1 l2
 *       | ((a1, ct1) :: t1, (a2, ct2) :: t2) when a2 < a1 -> Add (a2, ct2) :: diff l1 t2
 *       | _ -> failwith "I'm bad with arithmetic (This should not be possible)"
 *     in
 *     diff l1 l2
 *
 *   let diff (e1 : Fragment.Env.t) (e2 : Fragment.Env.t) =
 *     let n1 = Vec.length e1.frags in
 *     let n2 = Vec.length e2.frags in
 *     let new_frags =
 *       if n2 < n1 then begin
 *         warn
 *           "StateDiff: less fragment in the new state than in the old one. Deleted fragment not \
 *            shown";
 *         0
 *       end
 *       else n2 - n1
 *     in
 *     let modifs =
 *       Seq.iota n1
 *       |> Seq.filter_map (fun i ->
 *              match frag_diff (Fragment.Env.get e1 i) (Fragment.Env.get e2 i) with
 *              | [] -> None
 *              | l -> Some (i, l))
 *       |> List.of_seq
 *     in
 *     let news = Vec.to_seqi_sub e2.frags ~pos:n1 ~len:new_frags |> List.of_seq in
 *     { modifs; news }
 *
 *   open PP
 *
 *   let pp_modif_elem = function
 *     | Add (i, ctyp) -> prefix 2 1 (!^"At " ^^ shex i ^^ !^" add:") (Ctype.pp ctyp)
 *     | Mod (i, ctyp) -> prefix 2 1 (!^"At " ^^ shex i ^^ !^" modify:") (Ctype.pp ctyp)
 *     | Rem i -> !^"Remove type at " ^^ shex i
 *
 *   let pp_modif num mel =
 *     prefix 2 1 (dprintf "Change on fragment %d:" num) (list pp_modif_elem mel)
 *
 *   let pp_new num frag = prefix 2 1 (dprintf "New fragment %d:" num) (Fragment.pp frag)
 *
 *   let pp (fd : t) =
 *     separate hardline (List.map (uncurry pp_modif) fd.modifs)
 *     ^^ hardline
 *     ^^ separate hardline (List.map (uncurry pp_new) fd.news)
 * end *)

(** The type of state diff *)
type t = {
  dst : State.id;
  src : State.id;
  regs : tval Reg.PMap.t;
  (* read_vars are explicitly note here as one can get the read var diff from mem *)
  asserts : exp list;
  mem : Mem.t;
  fenv : Fragment.Env.t;
}

(** Generic diff between two {!Reg.Map.t}.
    The result if a {!Reg.PMap.t} with bindings for all differing register.
    The bound value is the value in the new/second register map.
*)
let reg_map_diff (rm1 : 'a Reg.Map.t) (rm2 : 'a Reg.Map.t) =
  Reg.seq_all ()
  |> Seq.filter_map (fun reg ->
         let e1 = Reg.Map.get rm1 reg in
         let e2 = Reg.Map.get rm2 reg in
         if State.equal_tval e1 e2 then None
         else begin
           debug "%t and %t are different" PP.(top State.pp_tval e1) PP.(top State.pp_tval e2);
           Some (reg, e2)
         end)
  |> Reg.PMap.of_seq

(** Pretty-prints a state diff *)
let pp (sd : t) =
  let open PP in
  record "state diff"
    [
      ("from", State.Id.pp sd.src ^^ !^" -> " ^^ State.Id.pp sd.dst);
      ("regs", Reg.PMap.pp State.pp_tval sd.regs);
      ("memory", Mem.pp sd.mem);
      ("fenv", Fragment.Env.pp sd.fenv);
      ( "asserts",
        separate_map hardline (fun e -> prefix 2 1 !^"assert:" $ State.pp_exp e) sd.asserts );
    ]

(** Build the diff between two states.

    The first state is the old one and the second is the new one *)
let diff (s1 : state) (s2 : state) : t =
  let src = s1.id in
  let dst = s2.id in
  let regs = reg_map_diff s1.regs s2.regs in
  let asserts = list_diff State.equal_exp "asserts" s1.asserts s2.asserts in
  let mem = Mem.diff s1.mem s2.mem in
  let fenv = s2.fenv in
  let res = { src; dst; regs; asserts; mem; fenv } in

  debug "diffing\n%t\n\n%t\ninto\n%t\n"
    PP.(topi State.pp s1)
    PP.(topi State.pp s2)
    PP.(topi pp res);
  res
