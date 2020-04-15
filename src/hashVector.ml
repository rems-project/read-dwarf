(* The documentation is in the mli file *)

type 'a vector = 'a Vector.t

type 'a t = 'a option vector

let mem hv k =
  if k < 0 then Raise.inv_arg "HashVector.mem: negative index %d" k;
  k < Vector.length hv && Vector.unsafe_get hv k != None

exception Exists

let set hv k v : unit =
  if k < 0 then Raise.inv_arg "HashVector.set: negative index %d" k;
  if k >= Vector.length hv then Vector.ensure hv (k + 1) None;
  Vector.unsafe_set hv k (Some v)

let add (hv : 'a t) (k : int) (v : 'a) : unit = if mem hv k then raise Exists else set hv k v

let get_opt hv k =
  if k < 0 then Raise.inv_arg "HashVector.get_opt: negative index %d" k;
  if k >= Vector.length hv then None else Vector.unsafe_get hv k

let get hv k = match get_opt hv k with None -> raise Not_found | Some v -> v

let empty () : 'a t = Vector.empty ()

let pp conv hv =
  PP.(
    surround 2 0 !^"hv{"
      (hv
      |> Vector.mapi (fun i x -> (i, x))
      |> Vector.fold_left
           (fun l (i, x) ->
             match x with None -> l | Some a -> prefix 2 1 (int i ^^ !^" ->") (conv a) :: l)
           []
      |> separate (semi ^^ space)
      )
      !^"}")
