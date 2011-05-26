#lang typed/racket/base


(provide (all-defined-out))


;; A ValuesContext describes if a context either
;;    * accepts any number multiple values by dropping them from the stack.
;;    * accepts any number of multiple values by maintaining them on the stack.
;;    * accepts exactly n values, erroring out
(define-type ValuesContext (U 'tail
                              'drop-multiple 
                              'keep-multiple
			      Natural))


;; Linkage
(define-struct: NextLinkage ([context : ValuesContext]))
(define next-linkage/drop-multiple (make-NextLinkage 'drop-multiple))
(define next-linkage/expects-single (make-NextLinkage 1))
(define next-linkage/keep-multiple-on-stack (make-NextLinkage 'keep-multiple))



;; LabelLinkage is a labeled GOTO.
(define-struct: LabelLinkage ([label : Symbol]
			      [context : ValuesContext]))



;; Both ReturnLinkage and ReturnLinkage/NonTail deal with multiple
;; values indirectly, through the alternative multiple-value-return
;; address in the LinkedLabel of their call frame.
(define-struct: ReturnLinkage ([tail? : Boolean]))
(define return-linkage (make-ReturnLinkage #t))
(define return-linkage/nontail (make-ReturnLinkage #f))

(define-type Linkage (U NextLinkage
                        LabelLinkage
                        ReturnLinkage))
