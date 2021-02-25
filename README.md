# read-dwarf

read-dwarf is a tool for exploring, symbolically executing, and
validating ELF binaries generated from C, using DWARF debugging
information.  It is work in progress, not at present ready for
external use.



## Dependencies

### Build dependencies

There is one dependency that is not on opam: `isla-lang`. Go to [the `isla-lang`
repository](https://github.com/rems-project/isla-lang) and install the latest
version. `opam install .` should work there.

Then you can either install the default configuration of read-dwarf with opam,
with `opam install .` or just install other dependencies with
`opam install --deps-only .` and then follow the rest of the README.


### Run-time dependencies

You need a more or less recent version `z3`, if don't have it on your system
package manager, you can use `opam install z3`.

You also need to install `isla`.  Warning for
non-Rust users: you need to add `~/.cargo/bin` to your PATH or specify the
position of `isla-client` in `ISLA_CLIENT_PATH` for `read-dwarf` to find it.

You will also need to build `aarch64.ir` from the [`symbolic_rebased` branch
of sail-arm](https://github.com/rems-project/sail-arm/tree/symbolic_rebased)
and place it at the root of the directory or change the `arch-file` path in
[src/config/config.toml](https://github.com/rems-project/read-dwarf/blob/master/src/config/config.toml).

## Configuration

Currently, read-dwarf only supports compile-time (not run-time) selection of
architecture, through [Dune's virtual
modules](https://dune.readthedocs.io/en/stable/variants.html). See the
[Architecture](https://github.com/rems-project/read-dwarf/blob/master/doc/mlds/Architecture.mld)
for more details.

Other compile-time configuration options can be edited in the file
[src/confing/default.ml](https://github.com/rems-project/read-dwarf/blob/master/src/config/default.ml),
and accessed in the rest of the code through the
`Config` module. Runtime configuration is loaded from
[src/config/config.toml](https://github.com/rems-project/read-dwarf/blob/master/src/config/config.toml).

## Building

A simple `make` works. A `read-dwarf` symlink will then be created.

While developing, `make merlin` will only build the necessary parts for the
merlin plugin to work. It will fail later than plain `make` allowing a more
accurate linting.

## Installing

To install `read-dwarf`, you can do `dune install` without opam or
just use opam and do `opam install .`.

`dune uninstall` also work.

## Auto-formatting

To have auto-formatting you need exactly version 0.12 of `ocamlformat`.
For example one could do:

`opam pin ocamlformat 0.12` and `opam install ocamlformat`.

Then you can use `make format` to format your code.
Please always format before committing.
Ask everyone if you want to change ocamlformat options or bump the version.


## Documentation

<https://htmlpreview.github.io/?https://github.com/rems-project/read-dwarf/blob/master/doc/html/index.html>

`make doc` builds the automatic documentation that is then accessible from
[`doc/html/index.html`](https://htmlpreview.github.io/?https://github.com/rems-project/read-dwarf/blob/master/doc/html/index.html).

The `odoc` program (`opam install odoc`) is required.

Extra documents are available in the `doc` folder.




## Usage

You can run `./read-dwarf --help` to get the list of subcommand. For each
subcommand you can run `./read-dwarf subcommand --help` to learn more about
the subcommand

## Testing

You can run `make test` for self testing.
The libraries `ounit`, `qtest` and `qcheck` are additionally required.
You can get then from opam:

```
opam install qtest ounit qcheck
```

## Folder structure

 - `src`: The OCaml sources. Run `make doc` for details.
   - `analyse`: Code to make hyperlinked webpages or text files of the DWARF debug information
   - `arch`: [Dune virtual](https://dune.readthedocs.io/en/stable/variants.html)
      library to select architecture.
   - `ast`: Syntax-tree utilities and manipulations.
   - `bin`: executable and miscellaneous commands.
   - `config`: read-dwarf and isla configuration Toml files & infrastructure to handle them.
   - `ctype`: C type system defintion.
   - `dw`: DWARF debug info manipulation (wrapper around linksem).
   - `elf`: ELF manipulation (wrapper around linksem).
   - `gen`: OCaml code generated from Ott definitions of SMT-Lib expressions.
   - `isla`: modules for interacting with Isla.
   - `run`: modules to symbolically evaluate an instruction, block or function.
   - `state`: definition of abstract machine state and interface to it.
   - `tests`: QuickCheck tests for some modules.
   - `trace`: modules for working with instruction traces.
   - `utils`: utility functions not specific to read-dwarf.
   - `z3`: modules for interacting with Z3.
 - `tests`: Library test suite. It compiles as a single executable that performs the tests
 - `dune-project`: Global dune configuration
 - `read-dwarf.opam`: OPAM file. Generated by `dune-project`, edit `dune-project`.
 - `doc`: Documentation sources.
 - `test_asm`: Simple assembly test to test `run-bb` subcommand.


## Caching

Read-dwarf may create a cache in a directory named `.rdcache`. When searching
for a cache, read-dwarf will search if there already is a `.rdcache` directory
either in the current directory or one of its parent and use the closest one it
finds. If it finds none and need a cache, it will create a .rdcache in the
current directory. This directory contains several caches indexed by name.

Use `read-dwarf cache --clear name` to delete such a cache or
`read-dwarf cache --clear --a` to clear the whole `.rdcache` directory.
You also do `read-dwarf cache --list` to list existing caches.


## People

Thibaut Pérami, Dhruv Makwana, Neel Krishnaswami, and Peter Sewell


## Funding

This software was developed by the University of Cambridge Computer
Laboratory as part of the Rigorous Engineering of Mainstream Systems
(REMS) project.

The project has been partly funded by EPSRC grant EP/K008528/1.
This project has received funding from the European Research Council
(ERC) under the European Union's Horizon 2020 research and innovation
programme (grant agreement No 789108, ERC Advanced Grant ELVER).
This project has been partly funded by an EPSRC Doctoral Training studentship.
This project has been partly funded by Google.
