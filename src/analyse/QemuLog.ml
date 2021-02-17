(*****************************************************************************)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

(**    read qemu log file to display coverage in control-flow graphs         *)
open Utils

open CollectedType

let read_qemu_log an filename_qemu_log : bool array =
  let log_addresses : addr list =
    match read_file_lines filename_qemu_log with
    | Error s -> fatal "%s\ncouldn't read qemu log file: \"%s\"\n" s filename_qemu_log
    | Ok lines ->
        let parse_line (s : string) : natural option =
          (*         Printf.printf "%s  " s;*)
          match Scanf.sscanf s "0x%x:%s" (fun addr _ -> addr) with
          | addr ->
              (*Printf.printf "PARSED %s\n" (pp_addr (Nat_big_num.of_int addr));*)
              Some (Nat_big_num.of_int addr)
          | exception _ -> (*Printf.printf "NOT\n";*) None
        in
        List.filter_map parse_line (Array.to_list lines)
  in
  let size = Array.length an.instructions in
  let visited = Array.make size false in
  List.iter
    (function
      | addr -> (
          (*Printf.printf "%s" (pp_addr addr); *)
          match an.index_option_of_address addr with
          | Some k -> visited.(k) <- true
          | None -> ()
        ))
    log_addresses;
  visited
