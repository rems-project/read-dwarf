(* This module is about file management and helpers *)

(** Double the size of a bytes object *)
let double_byte b = Bytes.extend b 0 (Bytes.length b)

(** Read all it can in an in_channel until one read reads nothing *)
let read_all (i : in_channel) : string =
  let rec read_all_bytes i b pos len =
    let _ = assert (pos + len = Bytes.length b) in
    let res = input i b pos len in
    match res with
    | 0 -> Bytes.sub_string b 0 pos
    | x when x = len -> read_all_bytes i (double_byte b) (Bytes.length b) (Bytes.length b)
    | _ -> read_all_bytes i b (pos + res) (len - res)
  in
  read_all_bytes i (Bytes.create 100) 0 100

(** [read_file s] return the content of file s *)
let read_file (f : string) : string =
  let c = open_in f in
  Protect.protect (fun () -> read_all c) (fun () -> close_in c)

(** [write_file file cont] write cont as the content of file s which is overwritten if it exists *)
let write_file (filename : string) (content : string) : unit =
  let c = open_out filename in
  Protect.protect (fun () -> output_string c content) (fun () -> close_out c)

(** Reads a S-expression from the input, line by line *)
let read_sexp (i : in_channel) : string =
  let line0 = input_line i in
  if String.contains line0 '(' then begin
    let pnum = ref 0 in
    let count = String.iter (function '(' -> incr pnum | ')' -> decr pnum | _ -> ()) in
    count line0;
    if !pnum = 0 then line0
    else
      let buf = Buffer.create (2 * String.length line0) in
      Buffer.add_string buf line0;
      while !pnum > 0 do
        let line = input_line i in
        count line;
        Buffer.add_string buf line
      done;
      Buffer.contents buf
  end
  else line0
