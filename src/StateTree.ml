(** This module provides a tree of state to represent an unmerged execution *)

type 'a t = { state : State.t; data : 'a; rest : 'a t list }

let bars =
  "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"

let bars_length = String.length bars

let startbar n =
  if bars_length < n then Raise.fail "StateTree too deep, %d but limit is %d" bars_length n;
  PP.substring bars 0 n

let rec prefix_iter f t =
  f t.data t.state;
  List.iter (prefix_iter f) t.rest

let rec postfix_iter f t =
  List.iter (postfix_iter f) t.rest;
  f t.data t.state

(** Default iter when you don't care about order *)
let iter = prefix_iter

(** This is prefix, do a List.rev to get a postfix version *)
let rec map_to_list f t =
  let node = f t.data t.state in
  node :: List.concat_map (map_to_list f) t.rest

let pp ppa t =
  let open PP in
  let rec ppi ss t =
    let line =
      startbar ss ^^ star ^^ nbspace ^^ ppa t.data ^^ colon ^^ nbspace ^^ State.Id.pp t.state.id
    in
    let rest = List.mapi (fun i t -> ppi (ss + i) t) t.rest |> List.rev in
    line ^^ hardline ^^ separate empty rest
  in
  ppi 0 t

let pp_all ppa t =
  let open PP in
  pp ppa t
  ^^ (map_to_list (fun _ s -> prefix 4 1 (dprintf "State %d:" s.id) (State.pp s)) t
     |> separate hardline
     )
