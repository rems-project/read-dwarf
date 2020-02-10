# Build system and maintenance (optional but nice to have stuff)

 - Find a name other than "read-dwarf"
 - Switch to `Logs` for logging instead of mixing debug and warn.
 - Make all the test target in the Makefile work again ideally without requiring
   hafnium-verification-plan
 - (To discuss) Remove file that can be deterministically generated from the Makefile
 - Setup documentation generation with dune.
 - Improve linksem `Dwarf` module to handle Dwarf 5 if required.
 - Add the generation of `Og` target in addition to `O{0-2}` for testing
 - Try to have the program automatically create/cache the objdump instead
   of asking it as a command line parameter.
 - Package this thing with opam and dune to check it works.
 - Package isla-lang properly (need to change ott package also).

# Required Plumbing

 - isla
   - Use Alasdair code for socket interaction and make it work as with cmd interface
   - Write the system that caches results.
   - Move the initial state from isla to readDwarf
 - Z3
   - add the check function to check validity of a model (non-statisfiability of it's negation)
   - find a way to deal with end of expression thing on simplify
   - Make it so that z3 do not restart all the time but just once.

# Content

## Memory

### Simulation

 - Handle read-mem and write-mem calls
 - Memory is represented as a global trace: list of memory event from latest to oldest
   The only optimization is:
     if someone reads from constant memory (rodata) then they receive a concrete value.
       (This is mostly to handle naively jump tables)
     if they write to it: UB

#### C type and location inference
  - Basic C type inference on linear traces

### Matching
  - Finding the simulation relation on global variables.
  - First try : Just match elf symbols with size and index lower stack with start sp
    and handle all the rest symbolically.

## Control-flow management
  - Start by handling that the exponential way and wait until it blows up
  - Don't deal with loops for now


## Function calling API
  - Add a format to specify the calling convention

# Current task stack for Thibaut. This is the short term task list

It is a stack because the topmost element may explode into a lot of other elements.

- Update isla and fix stuff
- Add support for online isla
- Add support for running multiple instruction from an ELF file into a new subcommand
- Add support for state memory manipulation
- Add support for concrete to symbol offset rewrite 
  if concrete value is <sym+off> then it is replaced by (bvadd sym off)
- Add support for rodata detection
- Add support for parsing of isla memory operations
- Add support for memory operations execution in islaTrace
- Add support for memory manipulation in Z3. (Mem = Array Addr Byte) 
  includes support for more Z3 syntax, including custom types.

