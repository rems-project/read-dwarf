
BINS=test

TOOLCHAIN=aarch64-linux-gnu-

all: $(foreach BIN,$(BINS), build/$(BINS))
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
