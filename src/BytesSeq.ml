(** This module represent a byte sub view on a bytes object.
    Contrary to [Bytes] it is a non-owning immutable view.
    It do not prevent the original bytes from being modified,
    and the changes will be propagated in the view.
    It is additional sugar on top of Linksem's [Byte_sequence_wrapper]

    About all the suffixed function:
    - All iteration function without suffix do the expected operation on char (as single bytes)
    - All iteration function with suffix nle do the expected operation on a sequence of
      integers of n bits as read in little endian.
    - All iteration function with suffix nbe do the expected operation on a sequence of
      integers of n bits as read in big endian.
    - All iteration function with suffix nbs do the expected operation on a sequence of
      BytesSeq.t of length corresponding to n bits.
*)

(* TODO make this include manually and make a mli file *)
include Byte_sequence_wrapper

type t = byte_sequence

(** See [Bytes.blit] *)
let blit (bs : t) (srcoff : int) (dst : bytes) (dstoff : int) (len : int) =
  if srcoff < 0 || srcoff + len > bs.len then Raise.inv_arg "BytesSeq.blit : out of bounds "
  else Bytes.blit bs.bytes (bs.start + srcoff) dst dstoff len

let unsafe_get bs i = Bytes.unsafe_get bs.bytes (i + bs.start)

(** Convert a string to a BytesSeq.t as raw data *)
let of_string s =
  (* This is safe because a BytesSeq is immutable *)
  of_bytes (Bytes.unsafe_of_string s)

(** Convert a hex string like A4B767DF into a {!BytesSeq.t} *)
let of_hex hexstr : t =
  let len = String.length hexstr in
  if len mod 2 == 1 then failwith "from_hex, string length must be even";
  let reslen = len / 2 in
  let res = Bytes.create reslen in
  let inc = Scanf.Scanning.from_string hexstr in
  for i = 0 to reslen - 1 do
    Scanf.bscanf inc "%2x" (fun v -> Bytes.set_uint8 res i v)
  done;
  of_bytes res

(* TODO I need some external inline test system to avoid cyclic dependencies *)
(* let _ =
 *   Tests.add_test "BytesSeq.of_hex" (fun () ->
 *       let bs = of_hex "2a615B7c" in
 *       to_string bs = "*a[|") *)

(** Convert to a char array *)
let to_array bs = Array.init (length bs) (fun i -> unsafe_get bs i)

(** Convert from a char array *)
let of_array arr = of_bytes (Bytes.init (Array.length arr) (fun i -> Array.unsafe_get arr i))

(*****************************************************************************)
(*         Getters                                                           *)
(*****************************************************************************)

(** The size int bytes of Ocaml int *)
let int_bytes = if Sys.int_size <= 32 then 4 else 8

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

(** Extract a sub range of a byte sequence. This is O(1) *)
let sub bs start len : t =
  if start >= 0 && len >= 0 && start + len <= bs.len then
    { bytes = bs.bytes; start = bs.start + start; len }
  else Raise.inv_arg "ByteSeq.sub at %d of length %d but total size is %d" start len bs.len

(** Same as {!sub} but for bytes *)
let bytes_sub bytes start len : t = sub (of_bytes bytes) start len

(** This can instantiated to sub_getter 10 to have getter of BytesSeq.t of size 10 *)
let sub_getter len bs start = sub bs start len

(** Same as {!sub_getter} but for bytes *)
let bytes_sub_getter len bs start = bytes_sub bs start len

(** Take the first i bytes of the sequence *)
let front i bs =
  if i > bs.len || i < 0 then
    Raise.inv_arg "Cannot take the first %d bytes of a bytesseq of size %d" i bs.len
  else { bytes = bs.bytes; start = bs.start; len = i }

(** Take the last i bytes of the sequence *)
let back i bs =
  if i > bs.len || i < 0 then
    Raise.inv_arg "Cannot take the last %d bytes of a bytesseq of size %d" i bs.len
  else { bytes = bs.bytes; start = bs.start + bs.len - i; len = i }

(** Tells if a byteseq fits this size (bs.len mod size = 0) *)
let fit size bs = bs.len mod size = 0

(** Trail is the part of the byte seq that do not fit in the regular size pattern *)
let trail size bs = back (bs.len mod size) bs

(*****************************************************************************)
(*         Iteration                                                         *)
(*****************************************************************************)

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

let iter16bs f bs = gen_iter 2 (bytes_sub_getter 2) f bs

let iter32le f bs = gen_iter 4 Bytes.get_int32_le f bs

let iter32be f bs = gen_iter 4 Bytes.get_int32_be f bs

let iter32bs f bs = gen_iter 4 (bytes_sub_getter 4) f bs

let iter64le f bs = gen_iter 8 Bytes.get_int64_le f bs

let iter64be f bs = gen_iter 8 Bytes.get_int64_be f bs

let iter64bs f bs = gen_iter 8 (bytes_sub_getter 8) f bs

(*****************************************************************************)
(*         Folding                                                           *)
(*****************************************************************************)

(* Warning do not handle the end of the byteseq *)

let gen_fold_left iterf f a bs =
  let r = ref a in
  iterf (fun x -> r := f !r x) bs;
  !r

let fold_left f a bs = gen_fold_left iter f a bs

let fold_left16le f a bs = gen_fold_left iter16le f a bs

let fold_left16be f a bs = gen_fold_left iter16be f a bs

let fold_left16bs f a bs = gen_fold_left iter16bs f a bs

let fold_left32le f a bs = gen_fold_left iter32le f a bs

let fold_left32be f a bs = gen_fold_left iter32be f a bs

let fold_left32bs f a bs = gen_fold_left iter32bs f a bs

let fold_left64le f a bs = gen_fold_left iter64le f a bs

let fold_left64be f a bs = gen_fold_left iter64be f a bs

let fold_left64bs f a bs = gen_fold_left iter64bs f a bs

(*****************************************************************************)
(*        To list                                                            *)
(*****************************************************************************)

let gen_to_list folder bs = List.rev (folder (fun list i -> i :: list) [] bs)

let to_list bs = gen_to_list fold_left bs

let to_list16le bs = gen_to_list fold_left16le bs

let to_list16be bs = gen_to_list fold_left16be bs

let to_list16bs bs = gen_to_list fold_left16bs bs

let to_list32le bs = gen_to_list fold_left32le bs

let to_list32be bs = gen_to_list fold_left32be bs

let to_list32bs bs = gen_to_list fold_left32bs bs

let to_list64le bs = gen_to_list fold_left64le bs

let to_list64be bs = gen_to_list fold_left64be bs

let to_list64bs bs = gen_to_list fold_left64bs bs

(*****************************************************************************)
(*        Binary IO                                                          *)
(*****************************************************************************)

let output ochannel bs =
  output_substring ochannel (Bytes.unsafe_to_string bs.bytes) bs.start bs.len

let write = Files.write_bin output

let input ichannel = Files.input_bytes ichannel |> of_bytes

let read = Files.read_bin input

(*****************************************************************************)
(*        Pretty printing                                                    *)
(*****************************************************************************)

let pp bs = bs |> to_char_list |> List.map PP.byte |> PP.separate PP.space

let ppc bs = bs |> to_char_list |> List.map PP.byte |> PP.separate PP.empty

let pp16le bs : PP.document =
  PP.(
    let front = bs |> to_list16le |> List.map hex16 |> separate space in
    if fit 2 bs then front else front ^^ space ^^ pp @@ trail 2 bs)

let pp16be bs : PP.document =
  PP.(
    let front = bs |> to_list16be |> List.map hex16 |> separate space in
    if fit 2 bs then front else front ^^ space ^^ pp @@ trail 2 bs)

let pp32le bs : PP.document =
  PP.(
    let front = bs |> to_list32le |> List.map hex32 |> separate space in
    if fit 4 bs then front else front ^^ space ^^ pp @@ trail 4 bs)

let pp32be bs : PP.document =
  PP.(
    let front = bs |> to_list32be |> List.map hex32 |> separate space in
    if fit 4 bs then front else front ^^ space ^^ pp @@ trail 4 bs)

let pp64le bs : PP.document =
  PP.(
    let front = bs |> to_list64le |> List.map hex64 |> separate space in
    if fit 8 bs then front else front ^^ space ^^ pp @@ trail 8 bs)

let pp64be bs : PP.document =
  PP.(
    let front = bs |> to_list64be |> List.map hex64 |> separate space in
    if fit 8 bs then front else front ^^ space ^^ pp @@ trail 8 bs)
