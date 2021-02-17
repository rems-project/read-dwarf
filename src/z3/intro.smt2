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
