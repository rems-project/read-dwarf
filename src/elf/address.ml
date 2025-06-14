type t = {
  section : string;
  offset: int;
}

let pp addr = Pp.(!^(addr.section) ^^ !^"+" ^^ ptr addr.offset)

let of_linksem (section, offset) = { section; offset = Z.to_int offset }

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

let to_sym {section; offset} = Sym_ocaml.Num.Offset (section, Z.of_int offset)