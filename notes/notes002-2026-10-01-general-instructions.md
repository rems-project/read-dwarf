# Claude: general instructions the read-dwarf work since September 2026 was done under

Claude: the standing instructions the project owner gave during the work on
read-dwarf (branch `reloc-new-ps`) from 18 September to 1 October 2026, in
anonymised form, for another agent instance working on this code.  The
prompts are in `notes001-2026-10-01-instructions-given.md`; the fuller set of
rules for the linksem side is `linksem/notes/notes013-2026-10-01-general-instructions.md`,
and all of it applies here too.

- Plan first for anything beyond a small change: a note with the plan and its
  choice points, which the owner answers as `PS:` comments before the work is
  done.  Notes go in `notes/` as `notesNNN-YYYY-MM-DD-topic.md`; the notes
  about experiments made with read-dwarf live in the private test repository.
- Every commit message by the agent starts with `Claude: `; inserted comments
  are prefixed `Claude: `; a README largely written by the agent says so at
  the top.  Do not rewrite the owner's own text.
- read-dwarf follows linksem: the ELF and DWARF model, the symbolic
  resolution of relocatable objects, the Linux alternatives and jump-table
  models and the cpucap names all come from linksem (`~/linksem`, not a
  pinned copy); do not re-parse what linksem parses, and remove ad hoc
  parsers when linksem gains the model (as happened for the alternatives).
  When linksem's interfaces change, update read-dwarf in the same step and
  rebuild it.
- New code goes into the existing `analyse` library, as a subdirectory picked
  up by `include_subdirs` (for instance `analyse/pkvm-alternatives/`,
  `analyse/pkvm-jump-table/`), not into a new dune library or via
  `copy_files`.
- Options are added as command-line flags of `rd` with sensible defaults
  (`--analyse-computed-branches`, `--alternatives-section`, `--resolution-env`,
  `--skylight`, `--cfg-dot-file`, `-o` for the whole-file html), so that the
  tool can be tried on the pKVM object without code changes.
- The html output is judged in the browser: keep it small (no padding
  columns; the location-list column last), well-formed (escape properly),
  and with links relative to `--out-dir` and `--comp-dir`; new per-unit
  information (alternatives, relocations, jump labels) gets a line in the
  index and in each unit's page, and the inline rendering of an alternative
  or jump-label block keeps variable-location and inlining annotations
  outside the block.
- Test every change on the two standing tests of the private repository:
  the small smoke test at `-O0`/`-O2` (now also with DWARF 5) and `rd` on the
  pKVM hypervisor object, and check that the rendering is sensible (and,
  after a linksem-only change, unchanged).
