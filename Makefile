####################################################################################
#  BSD 2-Clause License                                                            #
#                                                                                  #
#  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   #
#  BSD 2-Clause License. This license does not apply to files outside these        #
#  directories.                                                                    #
#                                                                                  #
#  Copyright (c) 2020-2021 Thibaut Pérami                                          #
#  Copyright (c) 2020-2021 Dhruv Makwana                                           #
#  Copyright (c) 2019-2021 Peter Sewell                                            #
#  All rights reserved.                                                            #
#                                                                                  #
#  This software was developed by the University of Cambridge Computer             #
#  Laboratory as part of the Rigorous Engineering of Mainstream Systems            #
#  (REMS) project.                                                                 #
#                                                                                  #
#  The project has been partly funded by EPSRC grant EP/K008528/1.                 #
#  This project has received funding from the European Research Council            #
#  (ERC) under the European Union's Horizon 2020 research and innovation           #
#  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                #
#  This project has been partly funded by an EPSRC Doctoral Training studentship.  #
#  This project has been partly funded by Google.                                  #
#                                                                                  #
#  Redistribution and use in source and binary forms, with or without              #
#  modification, are permitted provided that the following conditions              #
#  are met:                                                                        #
#  1. Redistributions of source code must retain the above copyright               #
#     notice, this list of conditions and the following disclaimer.                #
#  2. Redistributions in binary form must reproduce the above copyright            #
#     notice, this list of conditions and the following disclaimer in              #
#     the documentation and/or other materials provided with the                   #
#     distribution.                                                                #
#                                                                                  #
#  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              #
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               #
#  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 #
#  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             #
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    #
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                #
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                #
#  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             #
#  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              #
#  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              #
#  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              #
#  SUCH DAMAGE.                                                                    #
#                                                                                  #
####################################################################################

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

apply-header:
	headache -c etc/headache_config -h LICENCE Makefile
	headache -c etc/headache_config -h LICENCE test_asm/Makefile
	headache -c etc/headache_config -h LICENCE test_asm/test.asm
	headache -c etc/headache_config -h LICENCE `find src -name '*.ml*'`
	headache -c etc/headache_config -h LICENCE `find src -name '*.ott'`
	headache -c etc/headache_config -h LICENCE `find src -name '*.toml'`
	headache -c etc/headache_config -h LICENCE `find src -name '*.awk'`
	headache -c etc/headache_config -h LICENCE `find src -name '*.html'`
	headache -c etc/headache_config -h LICENCE `find src -name '*.smt2'`

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
