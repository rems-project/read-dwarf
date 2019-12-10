let debug = ref true (*false*)
let print_string s = if !debug then Printf.printf "%s" s; flush stdout
let print_string2 s = if !debug then Printf.printf "%s" s; flush stdout
