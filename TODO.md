# Build system and maintenance (optional but nice to have stuff)

 - Switch to Cmdliner for argument parsing and add multiple subcommands (or do it with Stdlib.Arg)
 - Switch to `Logs` for logging instead of mixing debug and warn.
 - Make all the test target in the Makefile work again ideally without requiring hafnium-verification-plan
 - (To discuss) Remove file that can be deterministically generated from the Makefile
 - Setup automated formating with dune
 - Setup documentation generation with dune.
 - Improve linksem `Dwarf` module to handle Dwarf 5.
 - Add the generation of `Og` target in addition to `O{0-2}` for testing
 - Try to have the program automatically create/cache the objdump instead of asking it as a command line parameter.
 - Package this thing with opam and dune to check it works.
 - Package isla-lang properly (need to change ott package also).

# Required Plumbing

 - isla
   - Modify `isla` to allow online interaction
   - Write the code that do the interaction
   - Write the system that caches results.
 - Z3
   - Write the pipe code
   - Check we can send requests and read answers

# Content

 - Find a clean way to represent a state to the user.
 - Symbolically execute custom assembly
 - Try to set up the state to have a single branch for basic non-branching code.
   - Need to figure out the intended value of a lot of system registers
 - Try to do some matching of manual assembly
 - Try to do some matching of Ox and Oy of `tiny_test`.
 - Specify the function API
 - Handle control flow the exponential way
 - write the rest of this list

