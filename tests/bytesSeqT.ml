open Common

let has_even_len str = String.length str mod 2 = 0

let hex_digit =
  Q.make ~print:Print.char Gen.(oneof [numeral; char_range 'a' 'f'; char_range 'A' 'F'])

let hex_string = Q.string_gen hex_digit.gen

let is_hex = function 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true | _ -> false

let well_formed =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with well-formed inputs" hex_string (fun str ->
      has_even_len str ==> String.for_all is_hex str)

let odd_len =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with odd-length inputs" hex_string (fun str ->
      (not (has_even_len str))
      ==>
      try
        ignore (BytesSeq.of_hex str);
        false
      with Failure _ -> true)

let not_hex =
  QCT.make ~count:100 ~name:"BytesSeq.of_hex with not-hex inputs" Q.small_string (fun str ->
      (has_even_len str && not (String.for_all is_hex str))
      ==>
      try
        ignore (BytesSeq.of_hex str);
        false
      with
      | Stdlib.Scanf.Scan_failure _ -> true
      | _ -> false)

let tests = [well_formed; odd_len; not_hex]
