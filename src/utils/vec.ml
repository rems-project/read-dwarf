(* The documentation is in the mli file *)

open Res.Array

type 'a t = 'a Res.Array.t

let length = length

let empty = empty

let mem = mem

let map = map

let get = get

let unsafe_get = unsafe_get

let set = set

let unsafe_set = unsafe_set

let update v i f = get v i |> f |> set v i

let unsafe_update v i f = unsafe_get v i |> f |> unsafe_set v i

let copy = copy

let for_all = for_all

let exists = exists

let map = map

let mapi = mapi

let iter = iter

let iteri = iteri

let iter_until ~limit f vec =
  let limit = min limit (length vec) in
  for i = 0 to limit - 1 do
    unsafe_get vec i |> f
  done

let iteri_until ~limit f vec =
  let limit = min limit (length vec) in
  for i = 0 to limit - 1 do
    unsafe_get vec i |> f i
  done

let iteri_rev f vec =
  for i = length vec - 1 downto 0 do
    f i (unsafe_get vec i)
  done

let to_list = to_list

let fold_right = fold_right

let foldi_left f v a =
  let acc = ref a in
  iteri (fun i v -> acc := f i !acc v) v;
  !acc

let foldi_right f v a =
  let acc = ref a in
  iteri_rev (fun i v -> acc := f i v !acc) v;
  !acc

let to_listi v = foldi_right (fun i v a -> (i, v) :: a) v []

let fold_left = fold_left

let add_one = add_one

let remove_one = remove_one

let remove_n = remove_n

let to_array = to_array

let of_array = of_array

(* TODO optimize this *)
let ensure vec size v =
  let len = length vec in
  if size > len then
    for _ = len to size - 1 do
      add_one vec v
    done
  else ()

let keep vec size =
  if size = 0 then clear vec
  else
    let len = length vec in
    if size < len then remove_n vec (len - size) else ()

let resize vec size v =
  let len = length vec in
  if size < len then remove_n vec (len - size) else ensure vec size v

(* TODO optimize this *)
let map2 f veca vecb =
  let arra = to_array veca and arrb = to_array vecb in
  Array.map2 f arra arrb |> of_array

let map_mut f vec =
  let len = length vec in
  for i = 0 to len - 1 do
    unsafe_update vec i f
  done

let map_mut_until ~limit f vec =
  let limit = min limit (length vec) in
  for i = 0 to limit - 1 do
    unsafe_update vec i f
  done

let fill_all vec elem =
  let len = length vec in
  for i = 0 to len - 1 do
    unsafe_set vec i elem
  done

let insert vec pos elem =
  let len = length vec in
  if pos = len then add_one vec elem
  else begin
    add_one vec (unsafe_get vec (len - 1));
    for i = len - 2 downto pos do
      unsafe_get vec i |> unsafe_set vec (i + 1)
    done;
    unsafe_set vec pos elem
  end

let to_seq_sub vec ~pos ~len =
  let rec next len vec i () =
    if i < len then Seq.Cons (unsafe_get vec i, next len vec (i + 1)) else Seq.Nil
  in
  if pos < 0 || pos + len > length vec then Raise.inv_arg "Vec.to_seq_sub: invalid range";
  next (pos + len) vec pos

let to_seq vec =
  let len = length vec in
  to_seq_sub vec ~pos:0 ~len

let to_seqi_sub vec ~pos ~len =
  let rec next len vec i () =
    if i < len then Seq.Cons ((i, unsafe_get vec i), next len vec (i + 1)) else Seq.Nil
  in
  if pos < 0 || pos + len > length vec then Raise.inv_arg "Vec.to_seq_sub: invalid range";
  next (pos + len) vec pos

let to_seqi vec =
  let len = length vec in
  to_seqi_sub vec ~pos:0 ~len

let pp conv vec = Pp.(!^"vec" ^^ (vec |> to_array |> array conv))

let ppi conv vec = Pp.(vec |> to_list |> List.mapi (fun i v -> (int i, conv v)) |> Pp.mapping "")
