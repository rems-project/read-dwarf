####################################################################################
#  BSD 2-Clause License                                                            #
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
#  This project has been partly funded by EPSRC grant EP/K008528/1.                #
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

BINS=test

TOOLCHAIN=aarch64-linux-gnu-

CONFIG := ../src/config/config.toml

RD=dune exec read-dwarf -- run-block --config $(CONFIG) --len=24 --linear

all: $(foreach BIN,$(BINS), build/$(BIN))
.PHONY: all

build:
	mkdir -p build

define all_rule
build/$(1): build/$(1).o
	$(TOOLCHAIN)ld build/$(1).o -o build/$(1)

build/$(1).o: build $(1).asm
	$(TOOLCHAIN)as -g $(1).asm -o build/$(1).o
endef
$(foreach BIN,$(BINS),\
	$(eval $(call all_rule,$(BIN)))\
)

clean:
	rm -rf build

.PHONY: clean

define rd_rule
test_rd_$(1): build/$(1)
	$(RD) build/$(1) "_start" > /dev/null
.PHONY: test_rd_$(1)
endef
$(foreach BIN,$(BINS),\
	$(eval $(call rd_rule,$(BIN)))\
)

test_rd: $(foreach BIN,$(BINS), test_rd_$(BIN))
.PHONY: test_rd
