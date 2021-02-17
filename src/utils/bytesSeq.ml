(* The documentation is in the mli file *)

open Fun
include Byte_sequence_wrapper

type t = byte_sequence

let int_bytes = if Sys.int_size <= 32 then 4 else 8

let unsafe_get bs i = Bytes.unsafe_get bs.bytes (i + bs.start)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Hexadecimal conversions } *)

let to_hex bs =
  let len = length bs in
  let buf = Buffer.create (2 * len) in
  for i = bs.start to bs.start + bs.len - 1 do
    Printf.bprintf buf "%02x" (Char.code (Bytes.unsafe_get bs.bytes i))
  done;
  Buffer.contents buf

let to_hex_rev bs =
  let len = length bs in
  let buf = Buffer.create (2 * len) in
  for i = bs.start + bs.len - 1 downto bs.start do
    Printf.bprintf buf "%02x" (Char.code (Bytes.unsafe_get bs.bytes i))
  done;
  Buffer.contents buf

(** Convert a hex string like A4B767DF into a {!BytesSeq.t} *)
let of_hex hexstr : t =
  let len = String.length hexstr in
  if len mod 2 == 1 then failwith "from_hex, string length must be even";
  let reslen = len / 2 in
  let res = Bytes.create reslen in
  let inc = Scanf.Scanning.from_string hexstr in
  for i = 0 to reslen - 1 do
    Scanf.bscanf inc "%1x%1x" (fun u v -> Bytes.(set_uint8 res i ((u lsl 4) lor v)))
  done;
  of_bytes res

(*$= of_hex & ~printer:to_hex
     (of_hex "2a615B7c") (of_string "*a[|")
*)
(*$T of_hex
     (try ignore (of_hex "2\175"); false with Stdlib.Scanf.Scan_failure _ -> true)
     (try ignore (of_hex "7="); false with Stdlib.Scanf.Scan_failure _ -> true)
*)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Array conversions } *)

let to_array bs = Array.init (length bs) (fun i -> unsafe_get bs i)

let of_array arr = of_bytes (Bytes.init (Array.length arr) (fun i -> Array.unsafe_get arr i))

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Cutting the view } *)

let sub bs start len : t =
  if start >= 0 && len >= 0 && start + len <= bs.len then
    { bytes = bs.bytes; start = bs.start + start; len }
  else Raise.inv_arg "ByteSeq.sub at %d of length %d but total size is %d" start len bs.len

let front i bs =
  if i > bs.len || i < 0 then
    Raise.inv_arg "Cannot take the first %d bytes of a bytesseq of size %d" i bs.len
  else { bytes = bs.bytes; start = bs.start; len = i }

let back i bs =
  if i > bs.len || i < 0 then
    Raise.inv_arg "Cannot take the last %d bytes of a bytesseq of size %d" i bs.len
  else { bytes = bs.bytes; start = bs.start + bs.len - i; len = i }

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Interaction with [bytes] and raw string } *)

let blit (bs : t) (srcoff : int) (dst : bytes) (dstoff : int) (len : int) =
  if srcoff < 0 || srcoff + len > bs.len then Raise.inv_arg "BytesSeq.blit : out of bounds "
  else Bytes.blit bs.bytes (bs.start + srcoff) dst dstoff len

let of_string s =
  (* This is safe because a BytesSeq is immutable *)
  of_bytes (Bytes.unsafe_of_string s)

(** Same as {!sub} but for bytes *)
let bytes_sub bytes start len : t = sub (of_bytes bytes) start len

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Getters } *)

(** Get an Ocaml int at index in a [bytes].
    The size of the read is 4 or 8 depending of the size of the int

    The topmost bit is discarded
*)
let bytes_get_intle b i =
  if int_bytes = 4 then Int32.to_int @@ Bytes.get_int32_le b i
  else Int64.to_int @@ Bytes.get_int64_le b i

let gen_get size getter bs i =
  if 0 <= i && i <= bs.len - size then getter bs.bytes (bs.start + i)
  else Raise.inv_arg "ByteSeq: invalid access of length %d at %d but size is %d" size i bs.len

let get bs i = gen_get 1 Bytes.unsafe_get bs i

let get16le bs i = gen_get 2 Bytes.get_int16_le bs i

let get16be bs i = gen_get 2 Bytes.get_int16_be bs i

let get32le bs i = gen_get 4 Bytes.get_int32_le bs i

let get32be bs i = gen_get 4 Bytes.get_int32_be bs i

let get64le bs i = gen_get 8 Bytes.get_int64_le bs i

let get64be bs i = gen_get 8 Bytes.get_int64_be bs i

let getintle bs i = gen_get int_bytes bytes_get_intle bs i

let getbs ~len bs i = sub bs i len

let getbvle ~size bs i = gen_get (size / 8) (BitVec.bytes_load ~size) bs i

let gen_get_ze size getter bs i =
  if 0 <= i && i < bs.len then (
    let b = Bytes.make size '\x00' in
    let actual_size = min size (bs.len - i) in
    blit bs i b 0 actual_size;
    getter b 0
  )
  else Raise.inv_arg "ByteSeq: invalid access at %d but size is %d" i bs.len

(* TODO: All the other _ze accessors *)
let getintle_ze bs i = gen_get_ze int_bytes bytes_get_intle bs i

(** Same as {!sub_getter} but for bytes *)
let bytes_getbs len bs start = bytes_sub bs start len

(** Tells if a byteseq fits this size (bs.len mod size = 0) *)
let fit size bs = bs.len mod size = 0

(** Trail is the part of the byte seq that do not fit in the regular size pattern *)
let trail size bs = back (bs.len mod size) bs

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Iterators } *)

(* Warning do not handle the end of the byteseq *)

let gen_iter step getter f bs =
  let e = bs.start + bs.len - step in
  let index = ref bs.start in
  while !index <= e do
    f (getter bs.bytes !index);
    index := !index + step
  done

let iter f bs = gen_iter 1 Bytes.unsafe_get f bs

let iter16le f bs = gen_iter 2 Bytes.get_int16_le f bs

let iter16be f bs = gen_iter 2 Bytes.get_int16_be f bs

let iter32le f bs = gen_iter 4 Bytes.get_int32_le f bs

let iter32be f bs = gen_iter 4 Bytes.get_int32_be f bs

let iter64le f bs = gen_iter 8 Bytes.get_int64_le f bs

let iter64be f bs = gen_iter 8 Bytes.get_int64_be f bs

let iterbs ~len f bs =
  gen_iter len (bytes_getbs len) f bs;
  if not @@ fit len bs then f (trail len bs)

let gen_fold_left iterf f a bs =
  let r = ref a in
  iterf (fun x -> r := f !r x) bs;
  !r

let fold_left f a bs = gen_fold_left iter f a bs

let fold_left16le f a bs = gen_fold_left iter16le f a bs

let fold_left16be f a bs = gen_fold_left iter16be f a bs

let fold_left32le f a bs = gen_fold_left iter32le f a bs

let fold_left32be f a bs = gen_fold_left iter32be f a bs

let fold_left64le f a bs = gen_fold_left iter64le f a bs

let fold_left64be f a bs = gen_fold_left iter64be f a bs

let fold_leftbs ~len f a bs = gen_fold_left (iterbs ~len) f a bs

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 List conversions } *)

let gen_to_list folder bs = List.rev (folder (fun list i -> i :: list) [] bs)

let to_list bs = gen_to_list fold_left bs

let to_list16le bs = gen_to_list fold_left16le bs

let to_list16be bs = gen_to_list fold_left16be bs

let to_list32le bs = gen_to_list fold_left32le bs

let to_list32be bs = gen_to_list fold_left32be bs

let to_list64le bs = gen_to_list fold_left64le bs

let to_list64be bs = gen_to_list fold_left64be bs

let to_listbs ~len bs = gen_to_list (fold_leftbs ~len) bs

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Binary IO } *)

let output ochannel bs =
  output_substring ochannel (Bytes.unsafe_to_string bs.bytes) bs.start bs.len

let input ichannel = Files.input_bytes ichannel |> of_bytes

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Pretty Printing } *)

let pp bs = bs |> to_char_list |> List.map Pp.byte |> Pp.separate Pp.space

let ppc bs = bs |> to_hex |> Pp.string

let ppint bs = bs |> to_hex_rev |> Pp.string

let ppby ~by bs = bs |> to_listbs ~len:by |> Pp.separate_map Pp.space ppc

let ppbyint ~by bs = bs |> to_listbs ~len:by |> Pp.separate_map Pp.space (to_hex_rev %> Pp.string)
