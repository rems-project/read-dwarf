;;==================================================================================;;
;;  BSD 2-Clause License                                                            ;;
;;                                                                                  ;;
;;  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   ;;
;;  BSD 2-Clause License. This license does not apply to files outside these        ;;
;;  directories.                                                                    ;;
;;                                                                                  ;;
;;  Copyright (c) 2020-2021 Thibaut Pérami                                          ;;
;;  Copyright (c) 2020-2021 Dhruv Makwana                                           ;;
;;  Copyright (c) 2019-2021 Peter Sewell                                            ;;
;;  All rights reserved.                                                            ;;
;;                                                                                  ;;
;;  This software was developed by the University of Cambridge Computer             ;;
;;  Laboratory as part of the Rigorous Engineering of Mainstream Systems            ;;
;;  (REMS) project.                                                                 ;;
;;                                                                                  ;;
;;  The project has been partly funded by EPSRC grant EP/K008528/1.                 ;;
;;  This project has received funding from the European Research Council            ;;
;;  (ERC) under the European Union's Horizon 2020 research and innovation           ;;
;;  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                ;;
;;  This project has been partly funded by an EPSRC Doctoral Training studentship.  ;;
;;  This project has been partly funded by Google.                                  ;;
;;                                                                                  ;;
;;  Redistribution and use in source and binary forms, with or without              ;;
;;  modification, are permitted provided that the following conditions              ;;
;;  are met:                                                                        ;;
;;  1. Redistributions of source code must retain the above copyright               ;;
;;     notice, this list of conditions and the following disclaimer.                ;;
;;  2. Redistributions in binary form must reproduce the above copyright            ;;
;;     notice, this list of conditions and the following disclaimer in              ;;
;;     the documentation and/or other materials provided with the                   ;;
;;     distribution.                                                                ;;
;;                                                                                  ;;
;;  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              ;;
;;  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               ;;
;;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 ;;
;;  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             ;;
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    ;;
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                ;;
;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                ;;
;;  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;;
;;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              ;;
;;  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              ;;
;;  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              ;;
;;  SUCH DAMAGE.                                                                    ;;
;;                                                                                  ;;
;;==================================================================================;;

(define-sort Addr () (_ BitVec 64))
(define-sort Byte () (_ BitVec 8))
(define-sort Word16 () (_ BitVec 16))
(define-sort Word32 () (_ BitVec 32))
(define-sort Word64 () (_ BitVec 64))
(define-sort Mem () (Array Addr Byte))

(define-fun offset ((addr Addr) (off Int)) Addr
  (bvadd addr ((_ int2bv 64) off))
  )

(define-fun store16le ((m Mem) (addr Addr) (val Word16)) Mem
  (store (store m addr ((_ extract 7 0) val)) (offset addr 1) ((_ extract 15 8) val)))

(define-fun select16le ((m Mem) (addr Addr)) Word16
  (concat (select m (offset addr 1)) (select m addr)))

(define-fun aligned ((v Int) (addr Addr)) Bool
  (= addr (bvmul ((_ int2bv 64) v) (bvudiv addr ((_ int2bv 64) v)))))

;; TODO: add store
