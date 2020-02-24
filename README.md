# <Insert name here>

The goal of this project is to be able to symbolically execute piece of assembly
using the `isla` project and then prove (as) automatically (as possible)
that two different piece of assembly have the exact same behavior.

## Dependencies

You need the lastest version of `linksem` from github pinned with opam to build.

You also need to install `isla` and `isla-lang` via they respective Makefile.
Warning for non-Rust users: You need to add ~/.cargo/bin to your PATH
or specify the position of `isla-client` in `ISLA_CLIENT_PATH`

Finally you need `cmdliner`, `pprint`, `res` and `ocamlgraph` from opam:

`opam install cmdliner pprint res ocamlgraph`

## Building

`make` is good

## Auto-formatting

To have auto-formatting you need exactly version 0.12 of `ocamlformat`. For example on could do

`opam pin ocamlformat 0.12` and `opam install ocamlformat`.

Please always format before committing.
Ask you want to change ocamlformat options or bump the version


## Documentation

`make doc` builds the documentation that is then accessible from `doc.html`.

## Usage

You can run `./read-dwarf --help` to get the list of subcommand. For each subcommand you can
run `./read-dwarf subcommand --help` to learn more about the subcommand

## Testing

You can run `make test` for self testing.
