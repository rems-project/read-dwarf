# Build system and maintenance (optional but nice to have stuff)

 - Find a name other than "read-dwarf"
 - Make all the test target in the Makefile work again ideally without requiring
   hafnium-verification-plan. Basically understand and cleanup the Makefile

## Internal infrastructure

 - Move test_asm and tiny_tests into OUnit infrastructure
 - Try to have read-dwarf automatically create/cache the objdump instead
   of asking it as a command line parameter.
 - Add prefix to external output or error coming from servers like isla or z3 with sed
 - Think about removing PPrint and using StdLib.Format (and Fmt) instead
   - Need to test performance,
   - Need to swap ott pp generation to StdLib.Format, which is good for portability
   - PPrint is pervasive in REMS code including sail. Scrapping it is probably useless.
 - Stop using location annotation internally and use type annotations instead.
   The boundary between location annotation and type annotation need to be thought of.
 - Hashcons expressions and use memoisation for pure annoying functions like simplify and
   for equality testing
 - Do our own simplifier and stop relaying on Z3 `simplify`.
 - Do a rd2 command that basically does the same thing as rd but using internal data structure
   and not Linksem's data structures.
 - Do `run-func-rd2`.
 - Do a html command that basically does the same thing as rd2 but with a collapsible
   html file and way more information, in particular with an option to have
   the result of symbolic evaluation
 - Do a state invariant checker and assert it more or less regularly.

## Linksem performance

 - Performance after 27/03/2020 patch:
   - All non printing operation are generally way less than 1s. Possible improvements:
     - Use nats and not natural for DWARF offsets (and keeps natural for actual 64 bits addresses)
     Actual profiling (dump-dwarf use PPrint and not linksem pretty printers):
     - dump-dwarf in O0 : 0.3s
     - dump-dwarf in O2 : 0.7s
   - Printing operation are still slow because of string allocation.
     The profiling indicate the runs are heavily GC-bound
     - linksem-info on O0: 0.7s
     - linksem-info on O2: 4s
     - rd on O0: 1.5s
     - rd on O2: 1.6s
     We should use ropes like ocaml-ropes (Need to decide if Lem depends on that,
     linksem depends on that or we reimplement them in lem or linksem)

# Unassigned necessary tasks

 - isla
   - Move the initial state from isla to readDwarf (Peter's qemu work)

# Current task stacks for Dhruv
 - Add symbolic evaluation for Branch tables (probably in
   `Runner.execute_normal`, check if any of the next pc's are symbolic and get
   a list of concrete addressess from this code - depends on what the API looks
   like)
 - Ask Peter about details

# Current task stacks for Thibaut. This is the short term task list

 - Fork PPrint to be able to add characters other than space as part of the indentation

## Memory stack

 - Add support for concrete to symbol offset rewrite
   if concrete value is <sym+off> then it is replaced by (bvadd sym off)
 - Continue support for memory manipulation in Z3.
   includes support for more Z3 syntax, including custom types.

## Dwarf locations stack

 - Build a indexing structure that can take a pc and a static location an tell what variable
   is there (and thus later what CType)
   Important: This can be incomplete: If I ask for a var type and I get nothing then
   it's the same as any actually untyped location. We'll improve the location system as
   needed.

## Ctype stack

 - Design the type system: LONG
 - LONG: Do Ctype propagation with fallback
   - if propagation fails, fallback on dwarf information: Requires location indexing system

## Inlining and special instructions stack

 - Think about how to handle fake calls and special instructions like "smc"
 - Think about how to flag all "bl" about whether they are normal or special
   or alternatively create a fake instruction flow stream.

# Done list

 - Z3
   - Add the check function to check validity of a model (non-statisfiability of it's negation)
   - Find a way to deal with end of expression thing on simplify
   - Make it so that Z3 do not restart all the time but just once.
 - Internalize dwarf hierarchy and dump it with the dump-dwarf subcommand
 - Define a proper concept of static "location" like register whatever or memory whatever
   This location concept need to be integrated with isla states and use reg.ml register concepts
 - Simple location evaluator
 - Add logging system with backtraces on fatal errors.
 - Add a caching system to store computed trace on disk
 - Add a preprocessing system to shorten the trace output by isla
 - Implement the C types from the notes and do the DWARF to internal type conversion
 - Improve Linksem performance
 - Build the ABI concept into the code, and add room for all Arch-dependent things
 - Do a start of the AArch64 ABI encoding sufficient to run a function.
 - Package isla-lang with dune and opam
 - Refactor the AST to no use isla-lang AST but our own.
 - Package read-dwarf with opam and dune.
 - Add a simplified trace on which to run the C type inference
 - Add the execution code (TraceRun) to run the simplified trace like IslaRun
 - Update BB and RunBB to use Trace and TraceRun
 - Handle the 52 bits real width pointer problem
 - Classify symbolic addresses between "concrete", "symbolic reg + concrete offset", and "other"
 - Add basic type inference to TraceRun.
 - Plug that into "run-func" (deadline on 24/04/2020)
 - Represent bitvectors in a clean way (with Zarith) and implement concrete evaluator
 - Add actual control-flow handling to TraceRun (computing the new PC in all cases)
 - Do the run-block command with breakpoints
 - Update the run-func command to use run-block and not run-bb as backend
 - Handle rodata reads: They stay in the trace for now
 - Handle typing global variable for C type inference.
 - Do a proper fetch and execute with global ELF and DWARF and multiple cached functions
 - Use that to allow "out of function" branches.
 - Add a new block end condition about running over a PC multiple times.
 - Add a clean concept of instruction footprint
 - Change run-func-rd from printing diffs to printing state part matching the footprints
 - Do state assertion removal (remove useless assertion according to Z3, stop executing
   if state is impossible to reach (dead code))
 - Overhaul memory representation to add restricted block (like the stack) and memory trace caches.
 - Resolve memory read to obviously non-aliasing addresses directly
 - Scrap Tests.ml completely and do an actual OUnit test suit that
   checks more that just "read-dwarf do not crash" on some inputs.
 - Fix up documentation links after major refactor
 - Figure out how to add extra documentation pages and add an overview and a pipeline page.
