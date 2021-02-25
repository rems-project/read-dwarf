(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module provide all pretty printing functionality. It's main goal is not
    directly handle the output but to handle how to layout complex data structure
    in a text format.

    The main idea of this library is to separate the layout description phase
    from the part where you actually lay out the thing to pretty print. Thus the
    two stage of pretty printing is first to generate a {!document} object
    describing the layout from the object to print and then render the document
    to the string using one of the printing

*)

(** This is a wrapper around the
    {{:http://cambium.inria.fr/~fpottier/pprint/doc/pprint/pprint/index.html} [pprint]} library *)
include PPrint

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Rendering documents }

    This section provide function to render documents in a text output.

    However unless you know what you are doing, you may prefer using the
    {!Logs} module with the function of "{!rendfmt}",
    instead of calling those function directly.
*)

(** Render the document in a pretty manner on the provided [out_channel] *)
let fprint (out : out_channel) (doc : document) = ToChannel.pretty 0.75 150 out doc

(** Render the document in a compact manner on the provided [out_channel] *)
let fprintln (out : out_channel) (doc : document) =
  fprint out (doc ^^ hardline);
  flush out

(** Print the document on [stdout]. The general way of  *)
let print doc = fprint stdout doc

(** Print the document on [stdout], then a endline, then flush *)
let println doc =
  fprint stdout (doc ^^ hardline);
  flush stdout

(** Print the document on [stderr], then a endline, then flush *)
let eprintln doc =
  fprint stderr (doc ^^ hardline);
  flush stderr

(** Print the document in a string, in a compact manner *)
let sprintc doc =
  let b = Buffer.create 50 in
  ToBuffer.compact b doc;
  Buffer.contents b

(** Print the document in a string, in a pretty manner *)
let sprint doc =
  let b = Buffer.create 50 in
  ToBuffer.pretty 0.75 150 b doc;
  Buffer.contents b

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {2:rendfmt Render documents in format strings }

    This sub section provide functions to embed a [pprint] in a usual
    [printf]-like format string.

    The general usage is to use the [%t] format and then appropriate mapping
    function. For example:

    {[Printf.printf "Documents: %t" Pp.(top printer object)]}

    This is lazy which means that if you use this in disabled debug messages,
    the object will not even be read. The only cost will be to allocate the
    closure on the minor GC.

    Wheter to use {!top} or {!tos} depends if the final output is an output
    stream or a plain string.*)

(** Convert a document printer to a Printf format string via [%t] for function
    outputing on an [out_channel] *)
let top pp obj o = fprint o (pp obj)

(** Same as {!top} but add 4 space of indentation everywhere on the object.
    The general use is:

    {[Printf.printf "Object:\n%t\n" Pp.(topi printer object)]}

    and it will print:

    {v Object:
    object line 1
    object line 2 v}

*)
let topi pp obj o = fprint o (nest 4 @@ blank 4 ^^ pp obj)

(** Convert a document printer to a Printf format string via [%t] for function
    outputing in a string *)
let tos pp obj () = sprint (pp obj)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Combinators }

    This section provide function to create documents from various object as well
    as combinators to generate documents from more complex objects.
*)

(** Printf like function that returns a document of the formatted string.
    In the end, it's just a string document, nothing more complex*)
let dprintf format = Printf.ksprintf ( !^ ) format

(** A breakable space *)
let space = break 1

(** A non-breakable space (equivalent to [char ' ']) *)
let nbspace = blank 1

(** Print a boolean as "true" or "false" *)
let bool b = if b then !^"true" else !^"false"

(** Print an int in decimal *)
let int i = i |> string_of_int |> string

(** Print an int in hexadecimal as if it was unsigned *)
let hex i = i |> Printf.sprintf "%x" |> string

(** Print an int in hexadecimal with a sign: [-1] will be printed to [-0x1] *)
let shex i =
  if i >= 0 then i |> Printf.sprintf "0x%x" |> string else -i |> Printf.sprintf "-0x%x" |> string

(** Print an int in hexadecimal with the [0x] prefix *)
let ptr i = i |> Printf.sprintf "0x%x" |> string

(** Print a byte as 2 hexadecimal digit *)
let byte c = c |> Char.code |> Printf.sprintf "%02x" |> string

(** Print an unsigned 16 bit integer as 4 hexadecimal digit *)
let hex16 i = i |> Printf.sprintf "%04x" |> string

(** Print an unsigned 32 bit integer as 8 hexadecimal digit *)
let hex32 i = i |> Printf.sprintf "%08lx" |> string

(** Print an unsigned 64 bit integer as 16 hexadecimal digit *)
let hex64 i = i |> Printf.sprintf "%016Lx" |> string

(** Print an array when provided with an element printer *)
let array = OCaml.array

(** Print an list when provided with an element printer *)
let list = OCaml.list

(** Print an option when provided with an element printer *)
let opt = OCaml.option

(** Print an pair when provided with both element printers *)
let pair conva convb (a, b) = OCaml.tuple [conva a; convb b]

(** Print an 3-sized tuple when provided with all three element printers *)
let tup3 conva convb convc (a, b, c) = OCaml.tuple [conva a; convb b; convc c]

(** Print a quoted string *)
let qstring s = s |> string |> dquotes

(** Ignore the input and print nothing *)
let erase _ = empty

(** Prints a mapping with this style:

    {v
name{
    key -> value;
    key -> value;
    key -> value;
} v}
*)
let mapping (name : string) (mappings : (document * document) list) : document =
  surround 2 0 (!^name ^^ !^"{")
    (List.map (fun (a, b) -> infix 2 1 !^"->" a b) mappings |> separate (semi ^^ space))
    !^"}"

(** Print a Hashtbl using {!mapping}*)
let hashtbl ?(name = "") key value ht =
  let bindings = Hashtbl.fold (fun i v l -> (key i, value v) :: l) ht [] in
  mapping name bindings

(** Print a sorted Hashtbl using {!mapping}*)
let hashtbl_sorted ~compare ?(name = "") key value ht =
  let bindings = Hashtbl.fold (fun i v l -> (i, v) :: l) ht [] in
  let sbindings =
    List.fast_sort (Pair.compare ~fst:compare ~snd:Fun.(const @@ const 0)) bindings
  in
  mapping name (List.map (Pair.map key value) sbindings)

(** Print a record in the format

    {v
name{
    field = value;
    field = value;
    field = value;
} v}
*)
let record name fields : document = !^name ^^ OCaml.record name fields

(** Like
    {{:http://cambium.inria.fr/~fpottier/pprint/doc/pprint/pprint/index.html#val-separate_map}
    [separate_map]} but with the index *)
let separate_mapi sep f l =
  match l with
  | [] -> empty
  | a :: l -> List.fold_left (fun (i, d) a -> (i + 1, d ^^ sep ^^ f i a)) (1, f 0 a) l |> snd

(** Concatenate the documents produced by the function over the array *)
let concat_array_map f a = Array.fold_left (fun d a -> d ^^ f a) empty a

(** Concatenate the document produced by the function on the array.
    The function also gets the index of the element *)
let concat_array_mapi f a =
  Array.fold_left (fun (i, d) a -> (i + 1, d ^^ f i a)) (0, empty) a |> snd

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Special printer } *)

(** Prints a [Unix.process_status] with an integer printer *)
let status iconv =
  Unix.(
    function
    | WEXITED a -> !^"Exited with " ^^ iconv a
    | WSIGNALED a -> !^"Signaled " ^^ iconv a
    | WSTOPPED a -> !^"Stopped by " ^^ iconv a)

(** Prints a [Unix.process_status] with decimal integers *)
let statusi = status int

(** Prints a [Unix.process_status] with hexadecimal integers *)
let statush = status hex
