#==================================================================================#
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
#  The project has been partly funded by EPSRC grant EP/K008528/1.                 #
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
#==================================================================================#

.global _start
.type _start, @function
_start:
  sub sp, sp, #16
  add x0, x0, #8
  add x0, x0, #8
# In case of problems, ensure x1, x2 are aligned
# Either with concrete addresses
# Or with SMT constraints - but how?
# (do we need to add the ability to send isla SMT constraints?)
# Or left-shifting the registers
  str x0, [x1]
  ldr x3, [x2]
  mul x5, x4, x3
  add sp, sp, #16

.size _start, .-_start
