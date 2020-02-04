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
 - Chose representation of memory in state
   - Global trace
   - Split on memory types ?

#### C type and location inference
  - Basic C type inference on linear traces
  - Memory types: 
    - rodata (read only static global),
    - data (mutable static globals, inlcude bss)
    - heap (managed by mpool, single block, should have exact same layout in both cases)
    - upper stack (stack above function entry sp: hopefully immutable)
    - lower stack (scratch value, hopefully we'll never match on this)
  - For now we'll only deal with lower stack

### Matching
  - Finding the simulation relation on global variables.
  - First try : Just match elf symbols with size and suppose heap has same layout.


## Control-flow management
  - Start by handling that the exponential way and wait until it blows up
  - Don't deal with loops for now


## Function calling API
  - Add a format to specify the calling convention
  - See if we need to deal with writing things on the ancestor stack or if we have that immutable.


## Loop management
  - TODO

