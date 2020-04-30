open Logs.Logger (struct
  let str = __MODULE__
end)

(* The value do not need to be normalized to any value to be valid.
   The representation is the same whatever the value is modulo 2^size,
   even for negative values.

   The normalized representation is between -2^(size -1) and 2^(size -1) -1.
*)
type t = { z : Z.t; size : int }

exception SizeMismatch of int * int

let get_same_size v v' =
  if v.size = v'.size then v.size else raise (SizeMismatch (v.size, v'.size))

let size v = v.size

(*****************************************************************************)
(*        Integer conversion                                                 *)
(*****************************************************************************)

let of_z ~size z = { z = Z.signed_extract z 0 size; size }

let of_int ~size i = i |> Z.of_int |> of_z ~size

let zero ~size = of_z ~size Z.zero

let empty = { z = Z.zero; size = 0 }

let one ~size = of_z ~size Z.one

let minus_one ~size = of_z ~size Z.minus_one

(* size is one *)
let of_bool b = if b then minus_one ~size:1 else zero ~size:1

let to_bool v = if v.size <> 1 then raise (SizeMismatch (v.size, 1)) else Z.testbit v.z 0

let to_z v = v.z

let to_uz v = Z.extract v.z 0 v.size

let to_int v = v |> to_z |> Z.to_int

let to_uint v = v |> to_uz |> Z.to_int

(*****************************************************************************)
(*        Bytes conversion                                                   *)
(*****************************************************************************)

let to_bytes v = v |> to_uz |> Z.to_bits |> Bytes.unsafe_of_string

let to_bytes_exact v =
  let string = v |> to_uz |> Z.to_bits in
  let target_size = (v.size + 7) / 8 in
  let res = Bytes.make target_size (Char.chr 0) in
  Bytes.blit_string string 0 res 0 (String.length string);
  res

let of_bytes ~size b = Z.of_bits (Bytes.unsafe_to_string b) |> of_z ~size

(*****************************************************************************)
(*        String conversion, printing                                        *)
(*****************************************************************************)

let of_string ?(base = 10) ~size s = of_z ~size (Z.of_string_base base s)

let of_substring ?(base = 10) ~size ~pos ~len s =
  of_z ~size (Z.of_substring_base ~pos ~len base s)

let to_string ?(base = 2) ?(unsigned = false) ?(prefix = false) v =
  let z = if unsigned then to_uz v else to_z v in
  if base = 10 then Z.to_string z
  else
    let (base_char, base_width) =
      match base with
      | 2 -> ('b', 1)
      | 8 -> ('o', 3)
      | 16 -> ('x', 4)
      | _ ->
          Raise.fail "bitVec.to_string: Only base 2,8,10 and 16 are supported but %d asked" base
    in
    let width = (v.size + base_width - 1) / base_width in
    let format_string =
      if prefix then Printf.sprintf "%%0#%d%c" (width + 2) base_char
      else Printf.sprintf "%%0%d%c" width base_char
    in
    Z.format format_string z

let of_smt s =
  let len = String.length s - 2 in
  assert (len >= 0);
  match s.[1] with
  | 'x' -> of_substring ~base:16 ~pos:2 ~len ~size:(len * 4) s
  | 'b' -> of_substring ~base:2 ~pos:2 ~len ~size:len s
  | _ -> Raise.fail "BitVec.of_smt: invalid format %s" s

let to_smt v =
  let base = if v.size mod 4 = 0 then 16 else 2 in
  let s = to_string ~base ~unsigned:true ~prefix:true v in
  let b = s |> Bytes.unsafe_of_string in
  Bytes.unsafe_set b 0 '#';
  Bytes.unsafe_to_string b

let pp_smt v = v |> to_smt |> PP.string

(*****************************************************************************)
(*        String conversion, printing                                        *)
(*****************************************************************************)

(* Arithmetic *)

let of_z_same f v v' =
  let size = get_same_size v v' in
  of_z ~size (f v.z v'.z)

let add v v' = of_z_same Z.add v v'

let sub v v' = of_z_same Z.sub v v'

let neg v = of_z ~size:v.size (Z.neg v.z)

let mul v v' =
  let size = get_same_size v v' in
  of_z ~size Z.(v.z * v'.z)

let sdiv v v' =
  if Z.zero = v'.z then v'
  else
    let size = get_same_size v v' in
    of_z ~size Z.(v.z / v'.z)

let udiv v v' =
  if Z.zero = v'.z then v'
  else
    let size = get_same_size v v' in
    let zu = to_uz v in
    let zu' = to_uz v' in
    of_z ~size Z.(zu / zu')

(*****************************************************************************)
(*        Bit manipulation                                                   *)
(*****************************************************************************)

let logand v v' = of_z_same Z.logand v v'

let logor v v' = of_z_same Z.logor v v'

let logxor v v' = of_z_same Z.logxor v v'

let lognot v = { v with z = Z.lognot v.z }

let redor v = if v.z = Z.zero then false else true

let redand v = if v.z = Z.minus_one then true else false

let shift_left v i = of_z ~size:v.size (Z.shift_left v.z i)

let shift_left_bv v v' = shift_left v (to_uint v')

let shift_right_arith v i = { v with z = Z.shift_right v.z i }

let shift_right_arith_bv v v' = shift_right_arith v (to_uint v')

let shift_right_logic v i = { v with z = Z.shift_right (to_uz v) i }

let shift_right_logic_bv v v' = shift_right_logic v (to_uint v')

let concat v v' =
  let u' = to_uz v' in
  let size = v.size + v'.size in
  { z = Z.((v.z lsl v'.size) lor u'); size }

let extract off endo v =
  let size = endo - off + 1 in
  { z = Z.signed_extract v.z off size; size }

let zero_extend m v = if m > 0 then { z = to_uz v; size = v.size + m } else v

let sign_extend m v = if m > 0 then { v with size = v.size + m } else v

(*****************************************************************************)
(*        Infix operators                                                    *)
(*****************************************************************************)

let ( + ) = add

let ( - ) = sub

let ( * ) = mul

let ( ~- ) = neg

let ( lsl ) = shift_left_bv

let asl = shift_left_bv

let ( lsr ) = shift_right_logic_bv

let ( asr ) = shift_right_arith_bv

let lnot = lognot

let ( land ) = logand

let ( lor ) = logor

let ( lxor ) = logxor

(*****************************************************************************)
(*        Tests                                                              *)
(*****************************************************************************)

let _ =
  Tests.add_test "BitVec.concat" (fun () ->
      let two = of_smt "#b0010" in
      let tree = of_smt "#b0011" in
      let res = of_smt "#b00100011" in
      concat two tree = res)

let _ =
  Tests.add_test "BitVec.shift_right_arith" (fun () ->
      shift_right_arith (of_smt "#b1100") 2 = minus_one ~size:4)

let _ =
  Tests.add_test "BitVec.sdiv" (fun () ->
      sdiv (of_int ~size:7 10) (of_int ~size:7 (-3)) = of_int ~size:7 (-3))

let _ = Tests.add_test "BitVec.oftosmt" (fun () -> to_smt (of_smt "#b1111") = "#xf")

let _ = Tests.add_test "BitVec.oftosmt2" (fun () -> to_smt (of_smt "#xcafebabe") = "#xcafebabe")

let _ = Tests.add_test "BitVec.oftosmt0" (fun () -> to_smt (of_smt "#x00000000") = "#x00000000")