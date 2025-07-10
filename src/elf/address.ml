type t = {
  section : string option;
  offset: int;
}

let absolute x = { section = None; offset = x }

let pp addr = Pp.(optional (fun s -> !^s ^^ !^"+") addr.section ^^ ptr addr.offset)

let of_linksem_relocatable (section, offset) = { section = Some section; offset = Z.to_int offset }

let (+) addr offset = { section = addr.section; offset = addr.offset + offset }

let compare f {section=s1; offset=o1} {section=s2; offset=o2} =
  if s1 = s2 then
    Some (f o1 o2)
  else
    None

let (<) = compare (<)

let (>) = compare (>)

let (<=) = compare (<=)

let (>=) = compare (>=)

let to_sym {section; offset} = 
  match section with
  | Some s -> Sym_ocaml.Num.Offset (s, Z.of_int offset)
  | None -> Sym_ocaml.Num.Absolute (Z.of_int offset)