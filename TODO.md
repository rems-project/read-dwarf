# Build system and maintenance (optional but nice to have stuff)

 - Find a name other than "read-dwarf"
 - Make all the test target in the Makefile work again ideally without requiring
   hafnium-verification-plan
 - Add the generation of `Og` target in addition to `O{0-2}` for testing
 - Package this thing with opam and dune to check it works.
 - Package isla-lang properly (need to change ott package also to include menhir lib).
   - Use dune for isla-lang

## Internal infrastructure

 - Try to have read-dwarf automatically create/cache the objdump instead
   of asking it as a command line parameter.
 - Switch to `Logs` for logging instead of mixing debug and warn.
 - Add prefix to external output or error coming from servers like isla or z3 with sed
 - Add test dependencies and ordering to Tests.ml
 - Add slow-test/fast test distinction (isla version is a slow test)
 - Add regexp test filtering
 - Think about removing PPrint and using StdLib.Format (and Fmt) instead
   - Need to test performance,
   - Need to swap ott pp generation to StdLib.Format, which is good for portability

## Linksem

 - Learn which test to run
 - Remove useless conversions in the Uint32 to Natural thing : Locally but not pushed
 - Fix the my_concat inefficiency : Locally but not pushed
 - See what could be moved to nat instead of natural
 - Current profiling state of dwarf\_extract\_static (with local patches)
   - Runtime on O0 hafnium only decreased from 4.8 to 4.3
   - About 60% of the time is spent in string copying, allocating and garbage collection
   - About 25% of the time is spent in big integers manipulation
   - A significant part of the gc work may be due to bigints and not to strings, I don't know yet.
   - About 5% of the time is spend directly in mydrop (and not in it's callees)
   - The rest may be useful computations.

     commenting out bits of harness_string_of_elf: 
     time make -B hafnium-O0/hafnium.linksem-info-test 
     24.19 s  for extract_dwarf, pp_fi,ld,li,is, pp_dwarf, mk_sdt, and pp_sdt
     19.6  s  for extract_dwarf, pp_fi,ld,li,is, pp_dwarf, mk_sdt, and pp_sdt  *
     15.28 s  for extract_dwarf,                 pp_dwarf, mk_sdt, and pp_sdt
     12.35 s  for extract_dwarf,                           mk_sdt, and pp_sdt
     12.20 s  for extract_dwarf,                           mk_sdt,           
      8.97 s  for extract_dwarf,                                             
      0.20 s  for                                                            

     * with the body of analyse_type_top replaced by a constant 

# Required Plumbing

 - isla
   - Write the system that caches results.
   - Move the initial state from isla to readDwarf

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

## Memory stack

- Handle the 52 bits real width pointer problem
- Add support for concrete to symbol offset rewrite
  if concrete value is <sym+off> then it is replaced by (bvadd sym off)
- Add support for rodata detection
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

# Done list

 - Z3
   - Add the check function to check validity of a model (non-statisfiability of it's negation)
   - Find a way to deal with end of expression thing on simplify
   - Make it so that Z3 do not restart all the time but just once.
