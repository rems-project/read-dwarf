let fatal0 fmt =
  Printf.printf fmt;
  flush stdout;
  exit 1

let fatal fmt s =
  Printf.printf fmt s;
  flush stdout;
  exit 1

let fatal2 fmt s t =
  Printf.printf fmt s t;
  flush stdout;
  exit 1

let nonfatal fmt s =
  Printf.printf fmt s;
  flush stdout

let nonfatal2 fmt s t =
  Printf.printf fmt s t;
  flush stdout
