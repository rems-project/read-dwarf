#!/bin/bash

set -euo pipefail

function target() {
    LEVEL="$1"
    echo "src/simrel/O$LEVEL.elf"
}

EXE="./read-dwarf"
CMD="run-func"

function save_state_tree_of_to() {
    LEVEL=$1
    SYMBOL=$2
    SAVE_FILE=$3
    echo $EXE $CMD "$(target $LEVEL)" "$SYMBOL" --dump-exec-tree "$SAVE_FILE"
    $EXE $CMD "$(target $LEVEL)" "$SYMBOL" --dump-exec-tree "$SAVE_FILE"
}

function run_examples_level() {
    LEVEL=$1
    save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test2 "./src/simrel/O${LEVEL}_test2.state"
    save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test3 "./src/simrel/O${LEVEL}_test3.state"
#   save_state_tree_of_to "$LEVEL" hyp_get_page_tv_test "./src/simrel/O${LEVEL}_test4.state"
}

run_examples_level "2"
run_examples_level "0"

