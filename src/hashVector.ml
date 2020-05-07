(* The documentation is in the mli file *)

type 'a t = 'a option Vec.t

let mem hv k =
  if k < 0 then Raise.inv_arg "HashVector.mem: negative index %d" k;
  k < Vec.length hv && Vec.unsafe_get hv k != None

exception Exists

let set hv k v : unit =
  if k < 0 then Raise.inv_arg "HashVector.set: negative index %d" k;
  if k >= Vec.length hv then Vec.ensure hv (k + 1) None;
  Vec.unsafe_set hv k (Some v)

let add (hv : 'a t) (k : int) (v : 'a) : unit = if mem hv k then raise Exists else set hv k v

let get_opt hv k =
  if k < 0 then Raise.inv_arg "HashVector.get_opt: negative index %d" k;
  if k >= Vec.length hv then None else Vec.unsafe_get hv k

let get hv k = match get_opt hv k with None -> raise Not_found | Some v -> v

let empty () : 'a t = Vec.empty ()

let pp conv hv =
  PP.(
    surround 2 0 !^"hv{"
      (hv
      |> Vec.mapi (fun i x -> (i, x))
      |> Vec.fold_left
           (fun l (i, x) ->
             match x with None -> l | Some a -> prefix 2 1 (int i ^^ !^" ->") (conv a) :: l)
           []
      |> separate (semi ^^ space)
      )
      !^"}")
