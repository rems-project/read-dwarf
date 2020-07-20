(** Miscellaneous types and utility functions used throughout the analyse code 
*)

type natural = Nat_big_num.num

(** machine address *)
type addr = natural

let pp_addr (a : natural) = Ml_bindings.hex_string_of_big_int_pad8 a

(** index into instruction-indexed arrays *)
type index = int

let measure_time = false

let time s f x =
  if measure_time then (
    let t1 : Unix.process_times = Unix.times () in
    let y = f x in
    let t2 : Unix.process_times = Unix.times () in
    Printf.printf "time %s user %6.3f  system %6.3f\n" s (t2.tms_utime -. t1.tms_utime)
      (t2.tms_stime -. t1.tms_stime);
    y
  )
  else f x

let rec list_last xs =
  match xs with
  | [x] -> x
  | _ :: (_ :: _ as xs'') -> list_last xs''
  | _ -> raise (Failure "list_last")

let rec list_last_opt xs =
  match xs with [x] -> Some x | _ :: (_ :: _ as xs'') -> list_last_opt xs'' | _ -> None

let char_list_of_string s =
  let n = String.length s in
  let rec f i = if i = n then [] else s.[i] :: f (i + 1) in
  f 0

let array_indices_such_that (f : 'a -> bool) (a : 'array) : int list =
  let len = Array.length a in
  let rec g acc k =
    if k >= len then acc else if f a.(k) then g (k :: acc) (k + 1) else g acc (k + 1)
  in
  g [] 0

(* in 4.10 *)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l
    )

let concat_map f l =
  let rec concat_map' f acc = function
    | [] -> List.rev acc
    | x :: l ->
        let xs = f x in
        concat_map' f (List.rev_append xs acc) l
  in
  concat_map' f [] l

(*****************************************************************************)
(**        read file of text lines                                            *)

(*****************************************************************************)

(** 'safe_open_in filename f' will open filename, pass it to f and cloth
    the channel at the end or when an exception is raised
    TODO use Protect.protect *)
let safe_open_in (filename : string) (f : in_channel -> 'a) : 'a =
  let chan = open_in filename in
  let res =
    try f chan
    with e ->
      close_in chan;
      raise e
  in
  close_in chan;
  res

type 'a ok_or_fail = Ok of 'a | MyFail of string

let read_file_lines (name : string) : string array ok_or_fail =
  let read_lines chan =
    let lines = ref [] in
    let () =
      try
        while true do
          lines := input_line chan :: !lines
        done
      with End_of_file -> ()
    in
    !lines |> List.rev |> Array.of_list
  in
  match safe_open_in name read_lines with
  | lines -> Ok lines
  | exception Sys_error s -> MyFail (Printf.sprintf "read_file_lines Sys_error \"%s\"\n" s)

(** escape HTML *)

let html_escape s =
  String.concat ""
    (List.map
       (fun c ->
         match c with
         | '&' -> "&amp;"
         | '<' -> "&lt;"
         | '>' -> "&gt;"
         | '\"' -> "&quot;"
         | '\'' -> "&apos;"
         | _ -> String.make 1 c)
       (char_list_of_string s))
