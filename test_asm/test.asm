.global test
.type test, @function
test:
  sub sp, sp, #16
  add x0, x0, #8
  add x0, x0, #8
  str x0, [x1]
  ldr x3, [x2]
  mul x5, x4, x3
  add sp, sp, #16

.size test, .-test
