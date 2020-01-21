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

(** `read_file s` return the content of file s *)
let read_file (f : string) : string =
  let c = open_in f in
  Protect.protect (fun () -> read_all c) (fun () -> close_in c)
