.global test
.type test, @function
test:
  sub sp, sp, #16
  add x0, x0, #8
  add x0, x0, #8
  add sp, sp, #16

.size test, .-test
