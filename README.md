# <Insert name here>

The goal of this project is to be able to symbolically execute binaries
using the `isla` project and then prove (as) automatically (as possible)
that two different optimisation levels have the exact same behavior. 
This is done by using intensively DWARF debugging information, in particular 
C type information. The project will only work for binaries generated from C.

## Dependencies

You need a specific `linksem` commit installed (See LINKSEM_COMMIT var in Makefile).
The simplest way is to check it out and then pin it with opam.

You also need to install `isla` and `isla-lang` via they respective Makefile.
Warning for non-Rust users: You need to add ~/.cargo/bin to your PATH
or specify the position of `isla-client` in `ISLA_CLIENT_PATH`

Finally you need `cmdliner`, `pprint`, `res` and `ocamlgraph` from opam:

`opam install cmdliner pprint res ocamlgraph`

## Building

`make` is good. `read-dwarf` symlink will then be created.

## Auto-formatting

To have auto-formatting you need exactly version 0.12 of `ocamlformat`. 
For example one could do:

`opam pin ocamlformat 0.12` and `opam install ocamlformat`.

Please always format before committing.
Ask everyone if you want to change ocamlformat options or bump the version.


## Documentation

`make doc` builds the documentation that is then accessible from `doc.html`.

## Usage

You can run `./read-dwarf --help` to get the list of subcommand. For each 
subcommand you can run `./read-dwarf subcommand --help` to learn more about
the subcommand

## Testing

You can run `make test` for self testing.

## Folder structure

 - `src`: The OCaml sources
   - `dune`: The dune configuration file
   - `.merlin`: Auto generated merlin file, do not commit.
   - `intro.smt2`: Common SMT definition used when calling a SMT solver.
     This is embedded in the binary.
 - `dune-project`: Global dune configuration
 - `notes`: Various notes and thought about the project development. 
 - `TODO.md`: The current TODO list.
 - `hafnium\*`: Each folder is a specific hafnium build to test on. 
   The binaries are frozen and should not be changed.
 - `compare-O0-O2`: Folder for graphs comparing `O0` and `O2` hafnium builds.
 - `test_asm`: Simple assembly test to test `run-bb` subcommand.
 - `tiny_test`: Simple C tests, see the corresponding README inside the folder.
 - `mpool`: Folder to build some mpool specific tests
 - `emacs-highlighting`: Emacs coloring for read-dwarf rd output. 
   Add the beginning of such file and follow the instructions.
 
