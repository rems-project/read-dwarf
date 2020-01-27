(** This module provide a structure to allow internalizing (with integers) of some type *)

type 'a vector = 'a Vector.t

type 'a bimap = { to_ident : ('a, int) Hashtbl.t; from_ident : 'a vector }

type 'a t = 'a bimap

(** Thrown when someone add something to a bimap and it already exists. *)
exception Exists

let make () : 'a bimap = { to_ident = Hashtbl.create 0; from_ident = Vector.empty () }

let add_ident bm v =
  if Hashtbl.mem bm.to_ident v then raise Exists
  else
    let ind = Vector.length bm.from_ident in
    Vector.add_one bm.from_ident v;
    Hashtbl.add bm.to_ident v ind;
    ind

let to_ident bm v = Hashtbl.find bm.to_ident v

let from_ident bm i = Vector.get bm.from_ident i

let mem bm v = Hashtbl.mem bm.to_ident v

let mem_id bm i = i < Vector.length bm.from_ident

let iter f bm = Hashtbl.iter f bm.to_ident

module PP = struct
  open PP

  let bimap conv hv =
    surround 2 0 !^"hv{"
      (hv
      |> Vector.mapi (fun i x -> prefix 2 1 (int i ^^ !^" ->") (conv x))
      |> Vector.to_list
      |> separate (semi ^^ space)
      )
      !^"}"
end
