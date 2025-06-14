include Elf.Address

let of_sym : Sym.t -> t = function
| Sym_ocaml.Num.Offset (section, offset) -> { section; offset = Z.to_int offset }
| _ -> Raise.fail "expected section+offset"

