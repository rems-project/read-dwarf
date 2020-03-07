//#include <stdio.h>   // not in Hafnium - added here for debug output

//#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>

#include <stdint.h>

/// This macro makes the compiler think a global variable is used
#define SINK(x) __asm__ volatile ("" : : "r"(&x) :)

#define NOINLINE __attribute__((noinline))

uint64_t x=6;

uint64_t addone (uint64_t i){
    return i + 1;
}

/// this is the testing function
NOINLINE uint64_t test() {

    x = 7;

    if (x==6)
      x=8;
    else
      x=9;
    uint64_t z = x;

    z += 34;
    z = addone(z);

    return z;
}

void main() {
    volatile uint64_t a = test();

    // Put all the sinks here
    SINK(x);

    // boilerplate for clean exit
#if   __ARM_ARCH == 7
    __asm__("mov r7, #1\n\t"
            "swi #0\n\t");
#elif __ARM_ARCH == 8
    __asm__("mov x0, #0\n\t"
            "mov x8, #93\n\t"
            "svc #0\n\t");
#elif __x86_64
    __asm__("mov $60, %rax\n\t"
            "mov $0, %rdi\n\t"
            "syscall \n\t");
#else
#error "only supporting ARMv7 and ARMv8 and x86_64"
#endif
}

