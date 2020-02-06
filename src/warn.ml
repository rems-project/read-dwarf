let fatal0 fmt =
  Printf.eprintf fmt;
  flush stderr;
  exit 1

let fatal fmt s =
  Printf.eprintf fmt s;
  flush stderr;
  exit 1

let fatal2 fmt s t =
  Printf.eprintf fmt s t;
  flush stderr;
  exit 1

let nonfatal0 fmt =
  Printf.eprintf fmt;
  flush stderr

let nonfatal fmt s =
  Printf.eprintf fmt s;
  flush stderr

let nonfatal2 fmt s t =
  Printf.eprintf fmt s t;
  flush stderr
