include Elf.Address

let of_sym : Sym.t -> t = function
| Sym_ocaml.Num.Offset (section, offset) -> { section; offset = Z.to_int offset }
| Sym_ocaml.Num.Absolute z -> absolute (Z.to_int z)

