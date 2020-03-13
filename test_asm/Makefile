
BINS=test

TOOLCHAIN=aarch64-linux-gnu-

RD=../read-dwarf run-bb -a ../aarch64.ir -s

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
	$(RD) build/$(1) > /dev/null
.PHONY: test_rd_$(1)
endef
$(foreach BIN,$(BINS),\
	$(eval $(call rd_rule,$(BIN)))\
)

test_rd: $(foreach BIN,$(BINS), test_rd_$(BIN))
.PHONY: test_rd
