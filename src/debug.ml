let debug = ref true (*false*)

let print_string s =
  if !debug then begin
    Printf.printf "%s" s;
    flush stdout
  end

let print_string2 s =
  if !debug then begin
    Printf.printf "%s" s;
    flush stdout
  end
