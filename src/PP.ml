(** This module provide base pretty printing, i.e pretty printing from external dependencies
    The other pretty printing convenience module are:
    - PPI : Pretty Printing intermediary : data structures
    - PPA : Pretty Printing all : everything
*)

include PPrint

(*****************************************************************************)
(*        Output                                                             *)
(*****************************************************************************)

let fprint out doc = ToChannel.pretty 0.75 100 out doc

let print doc = fprint stdout doc

let println doc =
  fprint stdout (doc ^^ hardline);
  flush stdout

let sprintc doc =
  let b = Buffer.create 50 in
  ToBuffer.compact b doc;
  Buffer.contents b

let sprint doc =
  let b = Buffer.create 50 in
  ToBuffer.pretty 0.75 100 b doc;
  Buffer.contents b

let fatal doc =
  fprint stderr (doc ^^ hardline);
  flush stderr;
  exit 1

(* Usage example :
   PP.(println @@ doc1 ^^ doc2)
   or
   PP.(fatal @@ doc1 ^^ doc2)
*)

(*****************************************************************************)
(*        Common                                                             *)
(*****************************************************************************)

let space = break 1

let nbspace = blank 1

let int i = i |> string_of_int |> string

let hex i = i |> Printf.sprintf "%x" |> string

let ptr i = i |> Printf.sprintf "0x%x" |> string

let byte c = c |> Char.code |> Printf.sprintf "%02x" |> string

let hex16 i = i |> Printf.sprintf "%04x" |> string

let hex32 i = i |> Printf.sprintf "%08lx" |> string

let hex64 i = i |> Printf.sprintf "%016Lx" |> string

let ( !$ ) = int

let array = OCaml.array

let list = OCaml.list

let opt = OCaml.option

let qstring s = s |> string |> dquotes

let erase _ = empty

let mapping (mappings : (document * document) list) =
  surround 2 0 !^"{"
    (List.map (fun (a,b) -> infix 2 1 !^"->" a b) mappings |> separate (semi ^^ space))
    !^"}"

(*****************************************************************************)
(*        Unix                                                               *)
(*****************************************************************************)

let status iconv =
  Unix.(
    function
    | WEXITED a -> !^"Exited with " ^^ iconv a
    | WSIGNALED a -> !^"Signaled " ^^ iconv a
    | WSTOPPED a -> !^"Stopped by " ^^ iconv a)

let statusi = status int

let statush = status hex

(* TODO Do a more careful inclusion *)
include Isla_lang.PP

let loc = Isla_lang.AST.pp_lpos

let lrng = Isla_lang.AST.pp_lrng
