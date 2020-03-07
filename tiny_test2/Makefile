# compilation for a tiny C example using compiler invocations based on those used by Hafnium

TEST=all

# The list of target architectures (native is the host architecture)
TARGETS=ARMv8-clang ARMv8-gcc native-gcc

# The list of program to test on
BINS=test

# The list of optimisation levels to try
OPTS=O0 O1 O2
# Example : ARMv8-gcc/test/O0.elf
# Generated targets:
# - Bin TARGET/BIN/OPT.elf
# - Obj TARGET/BIN/OPT.objdump-{d,DS}
# - RD TARGET/BIN/OPT.read-dwarf
# - All TARGET/BIN/OPT : all the previous

.PHONY: all bin
all: $(foreach TARGET,$(TARGETS),$(TARGET))
bin: $(foreach TARGET,$(TARGETS),$(TARGET)-bin)

# args: TARGET
define target_all_rule
.PHONY: $(1) $(1)-bin
$(1): $(foreach BIN, $(BINS),$(1)/$(BIN))

$(1)-bin: $(foreach BIN, $(BINS),$(1)/$(BIN)-bin)
endef
$(foreach TARGET,$(TARGETS),\
	$(eval $(call target_all_rule,$(TARGET)))\
)

# args: BIN
define bin_all_rule
.PHONY: $(1) $(1)
$(1): $(foreach TARGET, $(TARGETS),$(TARGET)/$(1))
$(1)-bin: $(foreach TARGET, $(TARGETS),$(TARGET)/$(1)-bin)
endef
$(foreach BIN, $(BINS),\
	$(eval $(call bin_all_rule,$(BIN)))\
)\

# args: TARGET BIN
define tbin_all_rule
.PHONY: $(1)/$(2) $(1)/$(2)-bin
$(1)/$(2): $(foreach OPT, $(OPTS),$(1)/$(2)/$(OPT))
$(1)/$(2)-bin: $(foreach OPT, $(OPTS),$(1)/$(2)/$(OPT)-bin)
endef
$(foreach TARGET,$(TARGETS),\
	$(foreach BIN, $(BINS),\
		$(eval $(call tbin_all_rule,$(TARGET),$(BIN)))\
	)\
)

# args: TARGET BIN OPT
define all_rule
.PHONY: $(1)/$(2)/$(3) $(1)/$(2)/$(3)-bin
ifeq ($(1),native-gcc)
$(1)/$(2)/$(3): $(1)/$(2)/$(3).objdump $(1)/$(2)/$(3).dwarfdump
else
$(1)/$(2)/$(3): $(1)/$(2)/$(3).objdump $(1)/$(2)/$(3).read-dwarf $(1)/$(2)/$(3).dwarfdump
endif

$(1)/$(2)/$(3)-bin: $(1)/$(2)/$(3).elf
endef

$(foreach TARGET,$(TARGETS),\
	$(foreach BIN, $(BINS),\
		$(foreach OPT, $(OPTS),\
			$(eval $(call all_rule,$(TARGET),$(BIN),$(OPT)))\
		)\
	)\
)

READ_DWARF=../read-dwarf

# the ARMv7 machinery is so that we (or Magnus/Tom) can try their decompilation tooling.  Not needed at present, but we might go back to it.
#ARMv7-gcc: ARMv7-gcc/$(TEST)-arm7-O0 ARMv7-gcc/$(TEST)-arm7-O1 ARMv7-gcc/$(TEST)-arm7-O2 ARMv7-gcc/$(TEST)-arm7-O0.read-dwarf ARMv7-gcc/$(TEST)-arm7-O1.read-dwarf ARMv7-gcc/$(TEST)-arm7-O2.read-dwarf


# for ARMv7 cross compiler:
# sudo apt-get inst$(TEST) gcc-arm-linux-gnueabihf
#
CROSS_COMPILE_ARMv7=arm-linux-gnueabihf-
CROSS_COMPILE_ARMv8=aarch64-linux-gnu-

GCC_ARMv7=$(CROSS_COMPILE_ARMv7)gcc -marm
GCC_ARMv8=$(CROSS_COMPILE_ARMv8)gcc
GCC_native=gcc
OBJDUMP7=$(CROSS_COMPILE_ARMv7)objdump
OBJDUMP8=$(CROSS_COMPILE_ARMv8)objdump


#CLANG_ARMv8=clang -target aarch64-none-elf -v

HAFNIUM_DIR=../../hafnium-playground-src
CLANG_DIR=$(HAFNIUM_DIR)/prebuilts/linux-x64/clang


########### CLANG PHASE 1:  .c TO LLVM BITCODE .bc ##############

# from hafnium-playground-src/out/reference/compile_commands.json
# command used to compile mpool.c
# clang -MMD -MF  aem_v8a_fvp_clang/obj/src/mm.mpool.o.d     -DVM_TOOLCHAIN=0 -DGIC_VERSION=3 -DGICD_BASE=0x2f000000 -DGICR_BASE=0x2f100000  -DHEAP_PAGES=60 -DMAX_CPUS=8 -DMAX_VMS=16 -DDEBUG=1  -I../../inc -I../../inc/vmapi -I../../src/arch/aarch64/inc -ffunction-sections -fdata-sections -flto -fno-builtin -ffreestanding -fpic -mcpu=cortex-a57+nofp -mstrict-align -target aarch64-none-eabi -fcolor-diagnostics -nostdinc -isystem/local/scratch/pes20/repos/hafnium-playground-src/prebuilts/linux-x64/clang/lib64/clang/8.0.4/include -isystem/local/scratch/pes20/repos/hafnium-playground-src/inc/system  -g -W$(TEST) -O2 -Wpedantic -Werror -fstack-protector-$(TEST)  -std=c11 -c ../../src/mpool.c -o  aem_v8a_fvp_clang/obj/src/mm.mpool.o

OUR_EXTRA_CFLAGS_CLANG=-Wno-comment -Wno-unused-variable -Wno-unused-function -static -save-temps=obj
CLANG_ARMv8=$(CLANG_DIR)/bin/clang  -DVM_TOOLCHAIN=0 -DGIC_VERSION=3 -DGICD_BASE=0x2f000000 -DGICR_BASE=0x2f100000  -DHEAP_PAGES=60 -DMAX_CPUS=8 -DMAX_VMS=16 -DDEBUG=1  -ffunction-sections -fdata-sections -flto -fno-builtin -ffreestanding -fpic -mcpu=cortex-a57+nofp -mstrict-align -target aarch64-none-eabi -fcolor-diagnostics -nostdinc -isystem $(CLANG_DIR)/lib64/clang/8.0.4/include -isystem $(HAFNIUM_DIR)/inc/system    -g -Wall -Wpedantic -Werror  -std=c11 -c $(OUR_EXTRA_CFLAGS_CLANG)



# removing these options:
#
#  -MMD -MF  aem_v8a_fvp_clang/obj/src/mm.mpool.o.d
# changing
# -I../../inc -I../../inc/vmapi -I../../src/arch/aarch64/inc
# to
# -I$(HAFNIUM_DIR)/inc -I$(HAFNIUM_DIR)/inc/vmapi -I$(HAFNIUM_DIR)/src/arch/aarch64/inc
# and
# -isystem/local/scratch/pes20/repos/hafnium-playground-src/inc/system
# to
# -isystem $(HAFNIUM_DIR)/inc/system
# ../../src/mpool.c
# -o  aem_v8a_fvp_clang/obj/src/mm.mpool.o
#
# -fstack-protector-$(TEST)

## ## ########### CLANG PHASE 2:  LLVM BITCODE .bc COMBINING FILES AND LTO? ###


## ## #from  hafnium-playground-src/out/reference/qemu_aarch64_clang/toolchain.ninja
## ## #  command = clang -MMD -MF ${out}.d     -DVM_TOOLCHAIN=0 -DGIC_VERSION=3 -DGICD_BASE=0x08000000 -DGICR_BASE=0x080A0000 ${defines} ${include_dirs} -ffunction-sections -fdata-sections -flto -fno-builtin -ffreestanding -fpic -mcpu=cortex-a57+nofp -mstrict-align -target aarch64-none-eabi -fcolor-diagnostics -nostdinc -isystem/local/scratch/pes20/repos/hafnium-playground-src/prebuilts/linux-x64/clang/lib64/clang/8.0.4/include -isystem/local/scratch/pes20/repos/hafnium-playground-src/inc/system ${asmflags} -c ${in} -o ${out}
## ##
## ## #from hafnium-playground-src/out/reference/qemu_aarch64_clang/obj/src/arch/aarch64/hypervisor/hypervisor.ninja
## ## defines=-DHEAP_PAGES=60 -DMAX_CPUS=8 -DMAX_VMS=16 -DDEBUG=1
## ## include_dirs=-I$(HAFNIUM_DIR)/inc -I$(HAFNIUM_DIR)/inc/vmapi -I$(HAFNIUM_DIR)/src/arch/aarch64/inc -I$(HAFNIUM_DIR)/src/arch/aarch64
## ## asmflags=
## ##
## ## CLANG_ARMv8_ASM=$(CLANG_DIR)/bin/clang  -DVM_TOOLCHAIN=0 -DGIC_VERSION=3 -DGICD_BASE=0x08000000 -DGICR_BASE=0x080A0000 ${defines} ${include_dirs} -ffunction-sections -fdata-sections -flto -fno-builtin -ffreestanding -fpic -mcpu=cortex-a57+nofp -mstrict-align -target aarch64-none-eabi -fcolor-diagnostics -nostdinc -isystem $(CLANG_DIR)/lib64/clang/8.0.4/include -isystem $(HAFNIUM_DIR)/inc/system ${asmflags} -c


########### CLANG PHASE 3:  LLVM BITCODE .bc TO ELF OBJECT ##############

# rule qemu_aarch64_clang_link
#   command = ld.lld -pie --gc-sections --defsym=ORIGIN_ADDRESS=0x40001000 -O2 -lto-O2 --icf=$(TEST) --fatal-warnings --color-diagnostics ${ldflags} -o ${output_dir}/${target_output_name}${output_extension} --start-group @${output_dir}/${target_output_name}${output_extension}.rsp --end-group
#
#hack:
ldflags=

CLANG_ARMv8_LLD=$(CLANG_DIR)/bin/ld.lld -pie --gc-sections --defsym=ORIGIN_ADDRESS=0x40001000  --icf=all --fatal-warnings --color-diagnostics ${ldflags} --defsym=_start=main
#
#-o ${output_dir}/${target_output_name}${output_extension} --start-group @${output_dir}/${target_output_name}${output_extension}.rsp --end-group
# -O2 -lto-O2




########### GCC #########################################################

# ARMv7 doesn't have a division instruction so we need libgcc to provide it
# for real mpool. Though not for hacked mpool
#NO_STDLIB7=-lgcc -nostartfiles
NO_STDLIB7= -nostartfiles
NO_STDLIB8= -nostdlib
NO_STDLIB_NATIVE= -nostdlib


# compile to ARM, O0, O1, and O2

# compiler flags for an RMEM-acceptable build
CFLAGS_GCC=-g -std=c11 -Wall -Wno-comment -Wno-unused-variable  -save-temps=obj   -Wa,-L -static -Wl,--defsym,_start=main -ffreestanding
# -static


#-D__THREAD_START_H=\"thread_start_aarch64.h\"



#LINKERFLAGS_CLANG=-Wl,--defsym,_start=main -Wa,-L
#-D__THREAD_START_H=\"thread_start_aarch64.h\"

###########ARMv7-gcc#############

ARMv7-gcc/gcc-version:
	mkdir -p ARMv7-gcc
	$(GCC_ARMv7) --version > ARMv7-gcc/gcc-version

ARMv7-gcc/objdump-version:
	mkdir -p ARMv7-gcc
	$(OBJDUMP7) --version > ARMv7-gcc/objdump-version

#args: BIN OPT
define ARMv7-gcc_rule
ARMv7-gcc/$(1)/$(2).elf: $(1).c ARMv7-gcc/objdump-version ARMv7-gcc/gcc-version
	mkdir -p ARMv7-gcc/$(1)
	$(GCC_ARMv7) $(CFLAGS_GCC) $(NO_STDLIB7) -$(2) -o ARMv7-gcc/$(1)/$(2).elf $(1).c
endef

$(foreach BIN, $(BINS),\
	$(foreach OPT, $(OPTS),\
		$(eval $(call ARMv7-gcc_rule,$(BIN),$(OPT)))\
	)\
)

###########ARMv8-gcc#############

ARMv8-gcc/gcc-version:
	mkdir -p ARMv8-gcc
	$(GCC_ARMv8) --version > ARMv8-gcc/gcc-version

ARMv8-gcc/objdump-version:
	mkdir -p ARMv8-gcc
	$(OBJDUMP8) --version > ARMv8-gcc/objdump-version

#args: BIN OPT
define ARMv8-gcc_rule
ARMv8-gcc/$(1)/$(2).elf: $(1).c ARMv8-gcc/objdump-version ARMv8-gcc/gcc-version
	mkdir -p ARMv8-gcc/$(1)
	$(GCC_ARMv8) $(CFLAGS_GCC) $(NO_STDLIB8) -$(2) -o ARMv8-gcc/$(1)/$(2).elf $(1).c
endef

$(foreach BIN, $(BINS),\
	$(foreach OPT, $(OPTS),\
		$(eval $(call ARMv8-gcc_rule,$(BIN),$(OPT)))\
	)\
)



###########native-gcc#############

native-gcc/gcc-version:
	mkdir -p native-gcc
	$(GCC_native) --version > native-gcc/gcc-version

native-gcc/objdump-version:
	mkdir -p native-gcc
	objdump --version > native-gcc/objdump-version

#args: BIN OPT
define native-gcc_rule
native-gcc/$(1)/$(2).elf: $(1).c native-gcc/objdump-version native-gcc/gcc-version
	mkdir -p native-gcc/$(1)
	$(GCC_native) $(CFLAGS_GCC) $(NO_STDLIB8) -$(2) -o native-gcc/$(1)/$(2).elf $(1).c
endef

$(foreach BIN, $(BINS),\
	$(foreach OPT, $(OPTS),\
		$(eval $(call native-gcc_rule,$(BIN),$(OPT)))\
	)\
)



###########ARMv8-clang#############

ARMv8-clang/clang-version:
	mkdir -p ARMv8-clang
	$(CLANG_ARMv8) --version > ARMv8-clang/clang-version

ARMv8-clang/objdump-version:
	mkdir -p ARMv8-clang
	$(OBJDUMP8) --version > ARMv8-clang/objdump-version

CLANGLTO-O1= -lto-O1
CLANGLTO-O2= -lto-O2

#args: BIN OPT
define ARMv8-clang_rule
ARMv8-clang/$(1)/$(2).elf: $(1).c ARMv8-clang/objdump-version ARMv8-clang/clang-version
	mkdir -p ARMv8-clang/$(1)
	$(CLANG_ARMv8) -$(2) -o ARMv8-clang/$(1)/$(2).bc $(1).c
	$(CLANG_ARMv8_LLD) -$(2) $(CLANGLTO-$(2)) ARMv8-clang/$(1)/$(2).bc -o ARMv8-clang/$(1)/$(2).elf
endef

$(foreach BIN, $(BINS),\
	$(foreach OPT, $(OPTS),\
		$(eval $(call ARMv8-clang_rule,$(BIN),$(OPT)))\
	)\
)


###########objdumps#############

OBJDUMP-ARMv8-gcc=$(OBJDUMP8)
OBJDUMP-ARMv8-clang=$(OBJDUMP8)
OBJDUMP-native-gcc=objdump

#args: TARGET BIN OPT
define objdump_rule

$(1)/$(2)/$(3).objdump-d: $(1)/$(2)/$(3).elf
	$(OBJDUMP-$(1)) -d $(1)/$(2)/$(3).elf > $(1)/$(2)/$(3).objdump-d

$(1)/$(2)/$(3).objdump-DS: $(1)/$(2)/$(3).elf
	$(OBJDUMP-$(1)) -DS $(1)/$(2)/$(3).elf > $(1)/$(2)/$(3).objdump-DS

$(1)/$(2)/$(3).objdump-t: $(1)/$(2)/$(3).elf
	$(OBJDUMP-$(1)) -t $(1)/$(2)/$(3).elf > $(1)/$(2)/$(3).objdump-t

$(1)/$(2)/$(3).objdump-decodedline: $(1)/$(2)/$(3).elf
	$(OBJDUMP-$(1)) --dwarf=decodedline $(1)/$(2)/$(3).elf > $(1)/$(2)/$(3).objdump-decodedline

.PHONY: $(1)/$(2)/$(3).objdump
$(1)/$(2)/$(3).objdump: $(1)/$(2)/$(3).objdump-d $(1)/$(2)/$(3).objdump-DS $(1)/$(2)/$(3).objdump-t $(1)/$(2)/$(3).objdump-decodedline
endef

$(foreach TARGET,$(TARGETS),\
	$(foreach BIN, $(BINS),\
		$(foreach OPT, $(OPTS),\
			$(eval $(call objdump_rule,$(TARGET),$(BIN),$(OPT)))\
		)\
	)\
)




###########read-dwarf#############

RDFLAGS=

#args: TARGET BIN OPT
define rd_rule
$(1)/$(2)/$(3).read-dwarf: $(1)/$(2)/$(3).objdump-d
	$(READ_DWARF) rd $(RDFLAGS) --objdump-d=$(1)/$(2)/$(3).objdump-d --elf=$(1)/$(2)/$(3).elf\
       --branch-tables=dummy.branch-tables > $(1)/$(2)/$(3).read-dwarf
endef

$(foreach TARGET,$(TARGETS),\
	$(foreach BIN, $(BINS),\
		$(foreach OPT, $(OPTS),\
			$(eval $(call rd_rule,$(TARGET),$(BIN),$(OPT)))\
		)\
	)\
)

###########dwarfdump#############

#args: TARGET BIN OPT
define dd_rule
$(1)/$(2)/$(3).dwarfdump: $(1)/$(2)/$(3).elf
	dwarfdump $(1)/$(2)/$(3).elf > $(1)/$(2)/$(3).dwarfdump
endef

$(foreach TARGET,$(TARGETS),\
	$(foreach BIN, $(BINS),\
		$(foreach OPT, $(OPTS),\
			$(eval $(call dd_rule,$(TARGET),$(BIN),$(OPT)))\
		)\
	)\
)



# use the linksem ELF+DWARF semantics to dump the DWARF info from each binary
# this won't work in general; it assumes linksem is checked out in a specific relative location

%.linksem-info: % ../../linksem/src/main_elf.opt
	../linksem/src/main_elf.opt --debug-dump=info $<  > $@





# TODO make emulation work again
# #starting with hafnium-playground/build_qemu_ramdisk/run_qemu.sh
# # qemu-system-aarch64 -M virt,gic_version=3 -cpu cortex-a57 -smp 8 -nographic -machine virtualization=true -kernel hafnium-playground-src/out/reference/qemu_aarch64_clang/hafnium.bin -initrd hafnium-build/initrd.img -append "rdinit=/sbin/init" -dtb hafnium-build/qemu_with_manifest.dtb

# run-qemu:
# 	qemu-aarch64 -cpu cortex-a57 ARMv8-clang/$(TEST)-arm8-O0
# 	qemu-aarch64 -cpu cortex-a57 ARMv8-gcc/$(TEST)-arm8-O0


# # in repos/sail-arm/arm-v8.5-a:    SAIL_DIR=~/repos/sail/ make aarch64
# SAIL_ARM_DIR=~/repos/sail-arm
# SAIL_ARM_C=$(SAIL_ARM_DIR)/arm-v8.5-a/aarch64
# run-sail:
# 	$(SAIL_ARM_C) -e ARMv8-clang/$(TEST)-arm8-O0
# 	$(SAIL_ARM_C) -e ARMv8-gcc/$(TEST)-arm8-O0



# # use the Cerberus C semantics to elaborate $(TEST).c into core and run it
# # this won't work in general; it assumes Cerberus is checked out in a specific relative location

# $(TEST).core:
# 	cd ~/repos/cerberus-private && ./cerberus --pp=core --exec ~/repos/hafnium-playground/mpool/$(TEST).c > ~/repos/hafnium-playground/mpool/$(TEST).core


# # compile on x86

# $(TEST)2:
# 	gcc spinlock.h mpool.h mpool.c main.c


.PHONY:clean
clean:
	rm -rf *~
	rm -rf *.gch
	rm -rf a.out
	rm -rf $(TARGETS)
