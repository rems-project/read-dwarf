(** This module provide base pretty printing, i.e pretty printing from external dependencies
    The other pretty printing convenience module are:
    - PPI : Pretty Printing intermediary : data structures
    - PPA : Pretty Printing all : everything
*)

include PPrint

let ( $ ) = ( @@ )

(*****************************************************************************)
(*        Output                                                             *)
(*****************************************************************************)

let fprint (out : out_channel) (doc : document) = ToChannel.pretty 0.75 150 out doc

let fprintln (out : out_channel) (doc : document) =
  fprint out (doc ^^ hardline);
  flush out

let print doc = fprint stdout doc

let println doc =
  fprint stdout (doc ^^ hardline);
  flush stdout

let eprintln doc =
  fprint stderr (doc ^^ hardline);
  flush stderr

let sprintc doc =
  let b = Buffer.create 50 in
  ToBuffer.compact b doc;
  Buffer.contents b

let sprint doc =
  let b = Buffer.create 50 in
  ToBuffer.pretty 0.75 150 b doc;
  Buffer.contents b

let fail doc = failwith (sprintc doc)

let fatal doc =
  fprint stderr (doc ^^ hardline);
  flush stderr;
  exit 1

(* Usage example :
   PP.(println $ doc1 ^^ doc2)
   or
   PP.(fatal $ doc1 ^^ doc2)
*)

(** To printf *)
let top pp obj o = fprint o (pp obj)

(** To sprintf *)
let tos pp obj () = sprint (pp obj)

(*
   Usage in printf familly function:
   lazy: Printf.printf "hey object %t is here" PP.(fun o -> fprint o $ doc1 ^^ doc2)

   direct: Printf.printf "hey object %a is here" PP.fprint PP.(doc1 ^^ doc2)

   the lazy version can be useful for logging message where you don't want to compute the document
   If the thing is never printed.

   Case when there is one object :
   Printf.printf "hey %t" PP.(top printer object)
*)

(*****************************************************************************)
(*        Common                                                             *)
(*****************************************************************************)

(** Printf like function that returns a document of the formatted string.
    it is just a string document nothing more complex*)
let dprintf format = Printf.ksprintf ( !^ ) format

let space = break 1

let nbspace = blank 1

let bool b = if b then !^"true" else !^"false"

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

let pair conva convb (a, b) = OCaml.tuple [conva a; convb b]

let tup3 conva convb convc (a, b, c) = OCaml.tuple [conva a; convb b; convc c]

let qstring s = s |> string |> dquotes

let erase _ = empty

let mapping (name : string) (mappings : (document * document) list) : document =
  surround 2 0 (!^name ^^ !^"{")
    (List.map (fun (a, b) -> infix 2 1 !^"->" a b) mappings |> separate (semi ^^ space))
    !^"}"

let hashtbl ?(name = "") key value ht =
  let res = ref [] in
  Hashtbl.iter (fun i v -> res := (key i, value v) :: !res) ht;
  mapping name (List.rev !res)

let record name fields : document = !^name ^^ OCaml.record name fields

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
