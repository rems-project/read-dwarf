(* BytesSeq Tests *)

module Gen = QCheck.Gen
module QCT = QCheck.Test

let ( ==> ) = QCheck.( ==> )

let has_even_len str = String.length str mod 2 = 0

let hex_arb =
  let hex_digit = Gen.(join @@ oneofl [numeral; char_range 'a' 'f'; char_range 'A' 'F']) in
  QCheck.make @@ Gen.small_string ~gen:hex_digit

let is_hex = function 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true | _ -> false

let bytes_seq_of_hex_well_formed =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with well-formed inputs" hex_arb (fun str ->
      has_even_len str ==> String.for_all is_hex str)

let bytes_seq_of_hex_odd_len =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with odd-length inputs" hex_arb (fun str ->
      (not (has_even_len str))
      ==>
      try
        ignore (BytesSeq.of_hex str);
        false
      with Failure _ -> true)

let bytes_seq_of_hex_not_hex =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with not-hex inputs" QCheck.small_string (fun str ->
      (has_even_len str && not (String.for_all is_hex str))
      ==>
      try
        ignore (BytesSeq.of_hex str);
        false
      with
      | Stdlib.Scanf.Scan_failure _ -> true
      | _ -> false)

let () =
  QCheck_base_runner.run_tests_main ~argv:Sys.argv
    [bytes_seq_of_hex_well_formed; bytes_seq_of_hex_odd_len; bytes_seq_of_hex_not_hex]
