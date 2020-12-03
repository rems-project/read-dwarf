#!/bin/bash

set -euo pipefail

LINUX_DIR="../linux"
KVM_DIR="arch/arm64/kvm/hyp/nvhe"
REL_ELF_PATH="$KVM_DIR/el2.o"
MAKEFILE="$LINUX_DIR/$KVM_DIR/Makefile"
function target() {
    LEVEL="$1"
    echo "vm-stuff/pkvm-O$LEVEL/el2.elf"
}

function copy_elf() {
    LEVEl="$1"
    sed -i "s/-O[[:digit:]]/-O$LEVEL/g" "$MAKEFILE"
    make -C "$LINUX_DIR" \
        ARCH=arm64 \
        CC=../linux-x86/clang-r383902/bin/clang \
        CROSS_COMPILE=aarch64-linux-gnu- \
        -j $(nproc) \
        "$REL_ELF_PATH"
    cp -a "$LINUX_DIR/$REL_ELF_PATH" "$(target $LEVEL)"
}


EXE="./read-dwarf"
CMD="run-func"

function save_state_tree_of_to() {
    LEVEL=$1
    SYMBOL=$2
    SAVE_FILE=$3
    $EXE $CMD --config src/config/config.toml "$(target $LEVEL)" "$SYMBOL" --dump-exec-tree "$SAVE_FILE"
}

function run_examples_level() {
    LEVEL=$1
    copy_elf "$LEVEL"
#   save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test2 "./src/simrel/O${LEVEL}_test2.state"
#   save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test3 "./src/simrel/O${LEVEL}_test3.state"
    save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test "./src/simrel/O${LEVEL}_test4.state"
}

run_examples_level "2"
run_examples_level "0"

