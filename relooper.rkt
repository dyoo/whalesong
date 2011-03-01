#lang typed/racket/base

(require "sets.rkt"
         racket/list
         racket/match)


;; What's the input?
;; What's the output?

;; A label has a name and ends with a branch.
(define-struct: label ([name : Symbol]
                       [code : Any]
                       [branch : Branch]
                       ;; The values below will be initialized.
                       [inlabels : (Setof label)]
                       [outlabels : (Setof label)]
                       [inlabels* : (Setof label)]
                       [outlabels* : (Setof label)])
  #:transparent)


(: new-label (Symbol Branch -> label))
;; Creates a label that's fairly uninitialized.
(define (new-label a-name a-branch)
  (make-label a-name #f a-branch (new-seteq) (new-seteq) (new-seteq) (new-seteq)))
  

;; A branch is either simple, or branching.
(define-type Branch (U Symbol        ;; simple, direct branch
                       #f            ;; leaf
                       branching))
(define-struct: branching ([consequent : Symbol]
                           [alternative : Symbol])
  #:transparent)




;; A soup is a set of labels.
(define-struct: soup ([labels : (HashTable Symbol label)]) 
  #:transparent)

(: new-soup ((Listof label) -> soup))
;; Constructs a new soup.
(define (new-soup labels)
  (let: ([ht : (HashTable Symbol label) (make-hash)])
        ;; First install the labels.
        (for-each (lambda: ([l : label])
                           (hash-set! ht (label-name l) l))
                  labels)
        ;; Next, initialize the in and out edges.
        (let: ([a-soup : soup (make-soup ht)])
              (for-each (lambda: ([l : label])
                                 (match (label-branch l)
                                   [(and n (? symbol?))
                                    (set-insert! (label-outlabels l) (soup-lookup a-soup n))
                                    (set-insert! (label-inlabels (soup-lookup a-soup n))
                                                 l)]
                                   ['#f
                                    (void)]
                                   [(struct branching (c a))
                                    (set-insert! (label-outlabels l) (soup-lookup a-soup c))
                                    (set-insert! (label-outlabels l) (soup-lookup a-soup a))
                                    
                                    (set-insert! (label-inlabels (soup-lookup a-soup c))
                                                 l)
                                    (set-insert! (label-inlabels (soup-lookup a-soup a))
                                                 l)]))
                        labels)
              a-soup)))


(: soup-lookup (soup Symbol -> label))
(define (soup-lookup a-soup a-name)
  (hash-ref (soup-labels a-soup) a-name))
  

;; What is a sample Soup?
(define a-soup (new-soup (list
                           (new-label 'ENTRY 'e2)
                           (new-label 'e2 (make-branching 'e5 'e12))
                           (new-label 'e5 'e9)
                           (new-label 'e9 'e2)
                           (new-label 'e12 #f))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-type Block (U basic-block
                      loop-block
                      multiple-block))

(define-struct: basic-block ([label : label] 
                             [next : Block]))

(define-struct: loop-block ([inner : Block]
                            [next : Block]))

(define-struct: multiple-block ([handled : (Listof Block)]
                                [next : Block]))