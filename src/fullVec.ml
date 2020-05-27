(* The documentation is in the mli file *)

open Fun

type 'a t = { vec : 'a Vec.t; mutable gen : int -> 'a }

let make gen =
  let vec = Vec.empty () in
  { vec; gen }

let copy fv = { fv with vec = Vec.copy fv.vec }

(* Fill vec until the index included *)
let rec fill_until fv n =
  let len = Vec.length fv.vec in
  if len <= n then begin
    Vec.add_one fv.vec (fv.gen len);
    fill_until fv n
  end
  else ()

let get fv i =
  if i < 0 then Raise.inv_arg "FullVector.get: Negative index";
  fill_until fv i;
  Vec.get fv.vec i

(*$Q get
   Q.small_nat (fun i -> let fv = make Fun.id in get fv i = i)
*)

let set fv i v =
  if i < 0 then Raise.inv_arg "FullVector.set: Negative index";
  if Vec.length fv.vec <= i then begin
    fill_until fv (i - 1);
    assert (Vec.length fv.vec = i);
    Vec.add_one fv.vec v
  end
  else Vec.set fv.vec i v

(*$Q set
    (Q.pair Q.pos_int Q.small_nat) (fun (v, i) -> let fv = make Fun.id in set fv i v; get fv i = v)
    Q.small_nat (fun i -> let fv = make Fun.id in set fv (i+1) 42; get fv i = i)
*)

let set_after fv i ngen =
  fill_until fv i;
  Vec.keep fv.vec i;
  fv.gen <- ngen

(*$Q set_after
    Q.small_nat (fun i -> let fv = make Fun.id in set_after fv i ((+) 1); get fv i = (i+1))
    Q.small_nat (fun i -> let fv = make Fun.id in set_after fv (i+1) ((+) 1); get fv i = i)
*)

let get_vec fv = fv.vec

let get_vec_until fv n =
  fill_until fv (n - 1);
  fv.vec

let map f fv =
  let vec = Vec.map f fv.vec in
  let gen = fv.gen %> f in
  { vec; gen }

let map_mut f fv =
  Vec.map_mut f fv.vec;
  fv.gen <- fv.gen %> f

let map_mut_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.map_mut f fv.vec

let iter_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.iter_until ~limit f fv.vec

let iteri_until ~limit f fv =
  fill_until fv (limit - 1);
  Vec.iteri_until ~limit f fv.vec

let pp conv fv =
  let open PP in
  Vec.to_listi fv.vec |> List.map (Pair.map int conv) |> mapping ""
