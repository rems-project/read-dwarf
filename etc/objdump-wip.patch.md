This is a beginning of the work to make the fetching of an objdump automatic.
Ideally read-dwarf will call objdump on ELF file it is called with and store
that in `.rdcache/objdumps`. Then no user will have to give the
`--objdump-d=...` option.

With this work, the goal is also to make the objdump parsing code in `Analyse`
more integrated, ideally by moving it into `Instr`: Processing an instruction
would also involve parsing its text representation.
