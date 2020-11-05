#!/bin/bash

set -euo pipefail

sed -i 's/-O[[:digit:]]/-O2/g' ../linux/arch/arm64/kvm/hyp/nvhe/Makefile
make -C ../linux ARCH=arm64 CC=../linux-x86/clang-r383902/bin/clang CROSS_COMPILE=aarch64-linux-gnu- -j $(nproc) arch/arm64/kvm/hyp/nvhe/el2.o
cp -a ../linux/arch/arm64/kvm/hyp/nvhe/el2.o vm-stuff/pkvm-O2/el2.elf
./read-dwarf run-func vm-stuff/pkvm-O2/el2.elf hyp_get_page_tv_test2 > ./src/simrel/O2_test2.state
./read-dwarf run-func vm-stuff/pkvm-O2/el2.elf hyp_get_page_tv_test3 > ./src/simrel/O2_test3.state

sed -i 's/-O[[:digit:]]/-O0/g' ../linux/arch/arm64/kvm/hyp/nvhe/Makefile
make -C ../linux ARCH=arm64 CC=../linux-x86/clang-r383902/bin/clang CROSS_COMPILE=aarch64-linux-gnu- -j $(nproc) arch/arm64/kvm/hyp/nvhe/el2.o
cp -a ../linux/arch/arm64/kvm/hyp/nvhe/el2.o vm-stuff/pkvm-O0/el2.elf
./read-dwarf run-func vm-stuff/pkvm-O0/el2.elf hyp_get_page_tv_test2 > ./src/simrel/O0_test2.state
./read-dwarf run-func vm-stuff/pkvm-O0/el2.elf hyp_get_page_tv_test3 > ./src/simrel/O0_test3.state
