include PPrint
include PPrintEngine
include PPrintRenderer

let space = break 1

let int i = i |> string_of_int |> string

let hex i = i |> Printf.sprintf "%x" |> string

let ( !$ ) = int

let fprint out doc = ToChannel.pretty 0.75 100 out doc

let print doc = fprint stdout doc

let println doc =
  fprint stdout (doc ^^ hardline);
  flush stdout

let array conv arr =
  surround 2 0 !^"[|" (arr |> Array.map conv |> Array.to_list |> separate (semi ^^ space)) !^"|]"

let qstring s = s |> string |> dquotes

let status iconv =
  Unix.(
    function
    | WEXITED a -> !^"Exited with " ^^ iconv a
    | WSIGNALED a -> !^"Signaled " ^^ iconv a
    | WSTOPPED a -> !^"Stopped by " ^^ iconv a)

let statusi = status int

let statush = status hex
