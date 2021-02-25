SHELL := /bin/bash

DUNE := $(shell which dune 2> /dev/null)
ifeq ($(DUNE),)
  $(error Cannot find dune, please install it or set DUNE to point to it)
endif

# The root repository of the linksem source
LINKSEM ?= ../linksem

# Output symlink name
OUT := read-dwarf
CONFIG := src/config/config.toml

default:
	@$(DUNE) build src/bin/main.exe
	@$(DUNE) build @install
	@ln -sf _build/default/src/bin/main.exe $(OUT)

.PHONY: default
.DEFAULT_GOAL: default

# This for easy use of the merlin fly-checker in IDEs
# It will just compute all cmi interfaces for type-checking but it will fail less
merlin:
	@$(DUNE) build @check

.PHONY: merlin

clean:
	@echo "Cleaning repository"
	@$(DUNE) clean
	@rm -rf $(OUT)
	@rm -rf doc.html
	@rm -rf .rdcache
	@rm -rf test_asm/.rdcache

.PHONY: clean

format:
	@echo "Formatting repository"
	@$(DUNE) build @fmt --auto-promote

.PHONY: format

doc:
	@echo "Generating documentation"
	@dune build @doc
	@cp -r _build/default/_doc/_html doc/html

.PHONY: doc

dune-test: default
	@echo "Running dune tests"
	@dune test -j 1 --no-buffer

.PHONY: dune-test

test: default
	@echo ""
	@echo "Checking dune tests"
	@dune test -j 1 --no-buffer
	@echo ""
	@echo "Checking Arithmetic test"
	./$(OUT) isla-test -s "add x0, x0, #8" > /dev/null
	./$(OUT) isla-test -s "mul x0, x1, x2" > /dev/null
	@echo "Arithmetic tests have passed"
	@echo ""
	@echo "Checking Memory test"
	./$(OUT) isla-test -s "ldr x0, [x1]" > /dev/null
	./$(OUT) isla-test -s "str x0, [x1]" > /dev/null
	@echo "Memory tests have passed"
	@echo ""
	@echo "Checking assembly tests (twice for cache testing)"
	make -C test_asm test_rd
	@echo "Assembly tests have passed"

.PHONY: test

%.objdumps:
	$(MAKE) $*.objdump-d $*.objdump-g $*.objdump-DS $*.objdump-x $*.objdump-t $*.objdump-dwarf-ait $*.objdump-rodata $*.hexdump-C $*.dwarfdump-a

%.objdump-d: %.elf
	aarch64-linux-gnu-objdump -d $< > $@

%.objdump-g: %.elf
	aarch64-linux-gnu-objdump -g $< > $@

%.objdump-DS: %.elf
	aarch64-linux-gnu-objdump -DS $< > $@

%.objdump-x: %.elf
	aarch64-linux-gnu-objdump -x $< > $@

%.objdump-t: %.elf
	aarch64-linux-gnu-objdump -t $< > $@

%.objdump-dwarf-ait: %.elf
	aarch64-linux-gnu-objdump --dwarf=abbrev,info,pubtypes $< > $@

%.objdump-rodata: %.elf Makefile
	aarch64-linux-gnu-objdump -D --section=.rodata $< > $@

%.hexdump-C: %.elf
	hexdump -C $< > $@

%.dwarfdump-a: %.elf
	dwarfdump -a $< > $@

%.objdump-bl-targets: %.elf
	grep "[[:space:]]bl[[:space:]]" $*.objdump-d | cut -f 4-| sort | uniq -c> $@

######################################################################

BEN=../system-litmus-harness/elfs/litmus
$(BEN).read-dwarf.html: $(BEN).elf
	./read-dwarf rd --elf $(BEN).elf --objdump-d $(BEN).objdump-d  --html --out $(BEN).read-dwarf.html

############### misc handy targets ###################################

%.dot.pdf %.dot.svg %.dot.xdot: %.dot
	dot -O -Tpdf -Tsvg -Txdot $<

######################################################################
##  other bits and bobs                                             ##
######################################################################

screenshot:

# to trace in Sail:
# in sail-arm
#   git checkout elf_trace
#   SAIL_DIR=~/repos/sail make aarch64 OPTS="-verbose 1"
#   ~/repos/sail-arm/arm-v8.5-a/aarch64 -e ARMv8-gcc/all-arm8-O0 -n 0x400890

########### compare die output of objdump and linksem ################

test-dies:
	aarch64-linux-gnu-objdump --dwarf=abbrev,info $(TEST) > test-dies-objdump
	$(LINKSEM)/src/main_elf.opt --debug-dump=dies $(TEST) > test-dies-linksem
	$(LINKSEM)/src/main_elf.opt --debug-dump=info $(TEST) > test-dies-linksem-info

GREPV=grep -v "Abbrev Number: 0" | grep -v "DW_AT_high_pc" | grep -v "DW_AT_lo_pc" | grep -v "DW_AT_inline" | grep -v "DW_AT_language" | grep -v "DW_AT_encoding"
diff:
	cat test-dies-objdump | $(GREPV) > test-dies-objdump.trimmed
	cat test-dies-linksem | $(GREPV) > test-dies-linksem.trimmed
	diff test-dies-objdump.trimmed test-dies-linksem.trimmed | more

######################################################################
