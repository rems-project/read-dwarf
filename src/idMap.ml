type ('a, 'b) t = { ids : ('a, int) Hashtbl.t; keys : 'a Vector.t; values : 'b Vector.t }

let make () = { ids = Hashtbl.create 10; keys = Vector.empty (); values = Vector.empty () }

exception Exists

let length m = Vector.length m.keys

let add m key value =
  if Hashtbl.mem m.ids key then raise Exists
  else
    let next_id = length m in
    Vector.add_one m.keys key;
    Vector.add_one m.values value;
    Hashtbl.add m.ids key next_id;
    next_id

let adds m key value = add m key value |> ignore

let to_ident m key = Hashtbl.find m.ids key

let of_ident m id = Vector.get m.keys id

let mem m key = Hashtbl.mem m.ids key

let mem_id m id = id >= 0 && id < length m

let geti m i = Vector.get m.values i

let unsafe_geti m i = Vector.unsafe_get m.values i

let getk m key =
  let id = to_ident m key in
  Vector.unsafe_get m.values id

let seti m i v = Vector.set m.values i v

let unsafe_seti m i v = Vector.unsafe_set m.values i v

let setk m key value =
  let id = to_ident m key in
  Vector.unsafe_set m.values id value

let fill_all m v = Vector.fill_all m.values v

let iter f m =
  let len = Vector.length m.values in
  for i = 0 to len - 1 do
    f (Vector.unsafe_get m.keys i) i (Vector.unsafe_get m.values i)
  done

let map_to_list f m =
  let len = Vector.length m.values in
  let res = ref [] in
  for i = 0 to len - 1 do
    res := f (Vector.unsafe_get m.keys i) i (Vector.unsafe_get m.values i) :: !res
  done;
  List.rev !res

let pp ?(name = "trimap") ~keys ~vals m =
  PP.(
    mapping name
    @@ map_to_list
         (fun key id value -> (group (prefix 2 1 $ int id ^^ dot $ keys key), vals value))
         m)
