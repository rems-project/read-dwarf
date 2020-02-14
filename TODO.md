# Build system and maintenance (optional but nice to have stuff)

 - Find a name other than "read-dwarf"
 - Make all the test target in the Makefile work again ideally without requiring
   hafnium-verification-plan
 - Setup documentation generation with dune.
 - Add the generation of `Og` target in addition to `O{0-2}` for testing
 - Package this thing with opam and dune to check it works.
 - Package isla-lang properly (need to change ott package also to include menhir lib).

## Internal infrastructure

 - Try to have read-dwarf automatically create/cache the objdump instead
   of asking it as a command line parameter.
 - Switch to `Logs` for logging instead of mixing debug and warn.
 - Add prefix to external output or error coming from servers like isla or z3 with sed
 - Add test dependencies and ordering to Tests.ml
 - Add test parameters to Test.ml
 - Add dumping of stack trace for unknown exceptions
 - Add slow-test/fast test distinction (isla version is a slow test)
 - Add regexp test filtering
 - Think about removing PPrint and using StdLib.Format (and Fmt) instead
   - Need to test performance,
   - Need to swap ott pp generation to StdLib.Format, which is good for portability

## Linksem

 - Learn which test to run
 - Remove useless conversions in the Uint32 to Natural thing
 - Fix the my_concat inefficiency
 - See what could be moved to nat instead of natural

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

# Current task stacks for Thibaut. This is the short term task list

- Add logging system with backtraces on fatal errors.
- Decide whether to stick with PPrint or swap to StdLib.Format
- Add support for running multiple instruction from an ELF file into a new subcommand



## Memory stack

- Add support for state memory manipulation
- Add support for concrete to symbol offset rewrite
  if concrete value is <sym+off> then it is replaced by (bvadd sym off)
- Add support for rodata detection
- Add support for parsing of isla memory operations
- Add support for memory operations execution in islaTrace
- Add support for memory manipulation in Z3. (Mem = Array Addr Byte)
  includes support for more Z3 syntax, including custom types.

## Ctype stack

- Design the internal concept a CType (Mostly based on Dwarf types)
- Internalize the Dwarf Ctype information for object symbols
- Lookup system to get the type of symbol +offset object
- Internalize the Dwarf Ctype information for function symbols
- General system to get quickly dwarf types of some machine position at any PC
  and in particular at function entry.
- Do basic Ctype propagation
- Fallback Ctype system: if propagation fails, fallback on dwarf information

## Control flow stack

- Build a trace tree from a set of normal traces
- Add isla support to set the pc before running an instruction
