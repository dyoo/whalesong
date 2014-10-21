#lang whalesong
(require (for-syntax racket))
(provide for/fold for*/fold
         for for*
         for/list for*/list
         for/vector for*/vector
         for/lists for*/lists
         for/and for*/and
         for/or for*/or
         for/first for*/first
         for/last for*/last
         for/sum for*/sum
         for/product for*/product
         for/hash for*/hash
         for/hasheq for*/hasheq
         for/hasheqv for*/hasheqv
         
         for/fold/derived for*/fold/derived
         (for-syntax split-for-body)
         (for-syntax (rename-out [expand-clause expand-for-clause]))
         
         (rename-out [*in-range in-range])
         (rename-out [*in-naturals in-naturals])
         (rename-out [*in-list in-list])
         ;(rename *in-mlist in-mlist)
         (rename-out [*in-vector in-vector])
         (rename-out [*in-string in-string])
         ;(rename *in-bytes in-bytes)
         ;(rename *in-stream in-stream)
         ;(rename *in-input-port-bytes in-input-port-bytes)
         ;(rename *in-input-port-chars in-input-port-chars)
         ;(rename *in-port in-port)
         ;(rename *in-lines in-lines)
         ;(rename *in-bytes-lines in-bytes-lines)
         ;in-hash
         ;in-hash-keys
         ;in-hash-values
         ;in-hash-pairs
         ;in-directory
         
         in-sequences
         in-cycle
         in-parallel
         in-values-sequence
         in-values*-sequence
         stop-before
         stop-after
         (rename-out [*in-producer in-producer])
         (rename-out [*in-indexed in-indexed])
         (rename-out [*in-value in-value])
         
         stream?
         stream-empty?
         stream-first
         stream-rest
         ;prop:stream
         stream-ref 
         ; stream-via-prop? ; only provided for racket/stream
         ;sequence->stream
         ;empty-stream 
         ; make-do-stream
         
         sequence?
         sequence-generate
         sequence-generate*
         ;prop:sequence
         
         define-sequence-syntax
         make-do-sequence
         :do-in
         
         define-in-vector-like
         define-:vector-like-gen
         (for-syntax make-in-vector-like
                     for-clause-syntax-protect))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sequence transformers:

(begin-for-syntax
  (struct sequence-transformer (expr-transformer clause-transformer))
  (define make-sequence-transformer sequence-transformer)
  (define (sequence-transformer-ref st i)
    (cond [(= i 0) (sequence-transformer-expr-transformer st)]
          [(= i 1) (sequence-transformer-clause-transformer st)]
          [else (error 'sequence-transformer-ref (~s "got: " i))]))
  
  #;(define-values (struct:sequence-transformer
                  make-sequence-transformer
                  sequence-transformer?
                  sequence-transformer-ref
                  sequence-transformer-set!)
    (make-struct-type 'sequence-transformer ; name
                      #f                    ; super-type
                      2                     ; init-field-count
                      0                     ; auto-field-count
                      #f                    ; auto-v
                      null                  ; props
                      (current-inspector)   ; inspector
                      0))                   ; proc-spec = 0 => acts as procedure
  
  (define (create-sequence-transformer proc1 proc2)
    (unless (and (procedure? proc1)
                 (or (procedure-arity-includes? proc1 1)
                     (procedure-arity-includes? proc1 0)))
      (raise-argument-error 'define-sequence-syntax
                            "(or/c (procedure-arity-includes/c 0) (procedure-arity-includes/c 1))"
                            0
                            proc1 proc2))
    (unless (and (procedure? proc2)
                 (procedure-arity-includes? proc2 1))
      (raise-argument-error 'define-sequence-syntax
                            "(procedure-arity-includes/c 1)"
                            1
                            proc1 proc2))
    (make-sequence-transformer
     (if (procedure-arity-includes? proc1 0)
         (lambda (stx)
           (if (identifier? stx)
               (proc1)
               (datum->syntax stx
                              ;; Use cons, not #`(#,op #,@args), to avoid replacing implicit #%app binding
                              (cons (proc1) (cdr (syntax-e stx)))
                              stx
                              stx)))
         proc1)
     proc2))
  
  (define (arm-for-clause clause cert)
    (define (map-cert s) (map cert (syntax->list s)))
    (syntax-case clause (:do-in)
      [[(id ...) (:do-in ([(outer-id ...) outer-expr] ...)
                         outer-check
                         ([loop-id loop-expr] ...)
                         pos-guard
                         ([(inner-id ...) inner-expr] ...)
                         pre-guard
                         post-guard
                         (loop-arg ...))]
       (with-syntax ([((outer-id ...) ...)
                      (map map-cert
                           (syntax->list #'((outer-id ...) ...)))]
                     [(outer-expr ...) (map-cert #'(outer-expr ...))]
                     [outer-check (cert #'outer-check)]
                     [(loop-expr ...) (map-cert #'(loop-expr ...))]
                     [pos-guard (cert #'pos-guard)]
                     [((inner-id ...) ...)
                      (map map-cert (syntax->list #'((inner-id ...) ...)))]
                     [pre-guard (cert #'pre-guard)]
                     [post-guard (cert #'post-guard)]
                     [(loop-arg ...) (map-cert #'(loop-arg ...))])
         #`[(id ...) (:do-in ([(outer-id ...) outer-expr] ...)
                             outer-check
                             ([loop-id loop-expr] ...)
                             pos-guard
                             ([(inner-id ...) inner-expr] ...)
                             pre-guard
                             post-guard
                             (loop-arg ...))])]
      [[(id ...) rhs]
       #`[(id ...) #,(cert #'rhs)]]
      [_
       ;; ill-formed clause...
       clause]))
  
  (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference)))
  
  (define (for-clause-syntax-protect clause)
    ;; This is slightly painful. The expansion into `:do-in' involves a lot
    ;; of pieces that are no treated as sub-expressions. We have to push the
    ;; taints down to all the relevant identifiers and expressions:
    (arm-for-clause clause syntax-arm))
  
  (define sequence-specialization-logger
    (make-logger 'sequence-specialization (current-logger)))
  
  (define (expand-clause orig-stx clause)
    (define (unpack stx)
      (syntax-case stx ()
        [[ids rhs] ; remove dye pack on `rhs' in case it's `(form . rest)'
         #`[ids #,(syntax-disarm #'rhs orig-insp)]]
        [_ stx]))
    (define (make-rearm)
      (syntax-case clause ()
        [(_ rhs)
         (lambda (stx)
           (syntax-rearm stx #'rhs))]))
    (let eloop ([use-transformer? #t])
      (define unpacked-clause (unpack clause))
      (syntax-case unpacked-clause (values in-parallel stop-before stop-after :do-in)
        [[(id ...) rhs]
         (let ([ids (syntax->list #'(id ...))])
           (for-each (lambda (id)
                       (unless (identifier? id)
                         (raise-syntax-error
                          #f
                          "expected an identifier to bind"
                          orig-stx
                          id)))
                     ids)
           (let ([dup (check-duplicate-identifier (syntax->list #'(id ...)))])
             (when dup
               (raise-syntax-error #f
                                   "duplicate identifier as sequence binding" orig-stx dup)))
           #f)
         'just-checking]
        [[(id ...) (form . rest)]
         (and use-transformer?
              (identifier? #'form)
              (sequence-transformer? (syntax-local-value #'form (lambda () #f))))
         (let ([m (syntax-local-value #'form)])
           (let ([xformer (sequence-transformer-ref m 1)]
                 [introducer (make-syntax-introducer)])
             (let ([xformed (xformer (introducer (syntax-local-introduce unpacked-clause)))])
               (if xformed
                   (let ([r (expand-clause orig-stx
                                           (arm-for-clause
                                            (syntax-local-introduce (introducer xformed))
                                            (make-rearm)))])
                     (syntax-property r
                                      'disappeared-use
                                      (cons (syntax-local-introduce #'form)
                                            (or (syntax-property r 'disappeared-use)
                                                null))))
                   (eloop #f)))))]
        [[(id ...) (:do-in . body)]
         (syntax-case #'body ()
           [(([(outer-id ...) outer-rhs] ...)
             outer-check
             ([loop-id loop-expr] ...)
             pos-guard
             ([(inner-id ...) inner-rhs] ...)
             pre-guard
             post-guard
             (loop-arg ...)) #'body]
           [else (raise-syntax-error #f "bad :do-in clause" orig-stx clause)])]
        [[(id) (values rhs)]
         (expand-clause orig-stx #'[(id) rhs])]
        [[(id ...) (in-parallel rhs ...)]
         (and (= (length (syntax->list #'(id ...)))
                 (length (syntax->list #'(rhs ...)))))
         ;; flatten in-parallel iterations:
         (with-syntax ([(((outer-binding ...)
                          outer-check
                          (loop-binding ...)
                          pos-guard
                          (inner-binding ...)
                          pre-guard
                          post-guard
                          (loop-arg ...)) ...)
                        (map (lambda (id rhs)
                               (expand-clause orig-stx #`[(#,id) #,rhs]))
                             (syntax->list #'(id ...))
                             (syntax->list #'(rhs ...)))])
           #`((outer-binding ... ...)
              (and outer-check ...)
              (loop-binding ... ...)
              (and pos-guard ...)
              (inner-binding ... ...)
              (and pre-guard ...)
              (and post-guard ...)
              (loop-arg ... ...)))]
        [[(id ...) (stop-before gen-expr pred)]
         (with-syntax ([((outer-binding ...)
                         outer-check
                         (loop-binding ...)
                         pos-guard
                         (inner-binding ...)
                         pre-guard
                         post-guard
                         (loop-arg ...))
                        (expand-clause orig-stx #`[(id ...) gen-expr])])
           #`((outer-binding ...)
              outer-check
              (loop-binding ...)
              pos-guard
              (inner-binding ...)
              (and pre-guard (not (pred id ...)))
              post-guard
              (loop-arg ...)))]
        [[(id ...) (stop-after gen-expr pred)]
         (with-syntax ([((outer-binding ...)
                         outer-check
                         (loop-binding ...)
                         pos-guard
                         (inner-binding ...)
                         pre-guard
                         post-guard
                         (loop-arg ...))
                        (expand-clause orig-stx #`[(id ...) gen-expr])])
           #`((outer-binding ...)
              outer-check
              (loop-binding ...)
              pos-guard
              (inner-binding ...)
              pre-guard
              (and post-guard (not (pred id ...)))
              (loop-arg ...)))]
        [[(id ...) rhs]
         #t
         (let ([introducer (make-syntax-introducer)])
           ;; log non-specialized clauses, for performance tuning
           (when (log-level? sequence-specialization-logger 'debug)
             (log-message sequence-specialization-logger
                          'debug
                          (format "non-specialized for clause: ~a:~a:~a"
                                  (syntax-source #'rhs)
                                  (syntax-line   #'rhs)
                                  (syntax-column #'rhs))
                          #'rhs))
           (with-syntax ([[(id ...) rhs] (introducer (syntax-local-introduce clause))])
             (arm-for-clause
              (syntax-local-introduce
               (introducer
                #`(([(pos->vals pos-next init pos-cont? val-cont? all-cont?)
                     #,(syntax-property
                        (syntax/loc #'rhs (make-sequence '(id ...) rhs))
                        'feature-profile:generic-sequence #t)])
                   (void)
                   ([pos init])
                   #,(syntax-property
                      (syntax/loc #'rhs (if pos-cont? (pos-cont? pos) #t))
                      'feature-profile:generic-sequence #t)
                   ([(id ...) #,(syntax-property
                                 (syntax/loc #'rhs (pos->vals pos))
                                 'feature-profile:generic-sequence #t)])
                   #,(syntax-property
                      (syntax/loc #'rhs (if val-cont? (val-cont? id ...) #t))
                      'feature-profile:generic-sequence #t)
                   #,(syntax-property
                      (syntax/loc #'rhs (if all-cont? (all-cont? pos id ...) #t))
                      'feature-profile:generic-sequence #t)
                   #,(syntax-property
                      (syntax/loc #'rhs ((pos-next pos)))
                      'feature-profile:generic-sequence #t))))
              (make-rearm))))]
        [_
         (raise-syntax-error #f
                             "bad sequence binding clause" orig-stx clause)]))))

(define-syntax (:do-in stx)
  (raise-syntax-error #f
                      "illegal outside of a loop or comprehension binding" stx))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  streams & sequences

;; structure type for generic sequences:
(define-values (struct:do-sequence
                make-do-sequence
                do-sequence?
                do-sequence-ref
                do-sequence-set!)
  (make-struct-type 'sequence #f 1 0 #f))

;; property for generic streams
(define stream-ref list-ref) ; xxx
#;(define-values (prop:stream stream-via-prop? stream-ref)
  (make-struct-type-property
   'stream
   (lambda (v si)
     (unless (and (vector? v)
                  (= 3 (vector-length v))
                  (procedure? (vector-ref v 0))
                  (procedure-arity-includes? (vector-ref v 0) 1)
                  (procedure? (vector-ref v 1))
                  (procedure-arity-includes? (vector-ref v 1) 1)
                  (procedure? (vector-ref v 2))
                  (procedure-arity-includes? (vector-ref v 2) 1))
       (raise-argument-error 'guard-for-prop:stream
                             (string-append
                              "(vector/c (procedure-arity-includes/c 1)\n"
                              "          (procedure-arity-includes/c 1)\n"
                              "          (procedure-arity-includes/c 1))")
                             v))
     (vector->immutable-vector v))))

;; new-style sequence property, where the property value is a procedure
;; to get the sequence-driving value and procedures;
;; this property is not currently exported
#;(define-values (prop:gen-sequence sequence-via-prop? sequence-ref)
  (make-struct-type-property
   'sequence
   (lambda (v si)
     (unless (and (procedure? v)
                  (procedure-arity-includes? v 1))
       (raise-argument-error 'guard-for-prop:sequence
                             "(procedure-arity-includes/c 1)"
                             v))
     v)))

;; exported sequence property, where the property value
;; is a procedure to get a sequence
#;(define-values (prop:sequence :sequence? :sequence-ref)
  (make-struct-type-property
   'sequence
   (lambda (v sinfo)
     (unless (and (procedure? v) (procedure-arity-includes? v 1))
       (raise-argument-error 'sequence-property-guard "(procedure-arity-includes/c 1)" v))
     (lambda (self)
       (let ([s (v self)])
         (unless (sequence? s)
           (raise-mismatch-error
            'sequence-generate
            "procedure (value of prop:sequence) produced a non-sequence: "
            s))
         s)))))

(define-syntax define-sequence-syntax
  (syntax-rules ()
    [(_ id expr-transformer-expr clause-transformer-expr)
     (define-syntax id
       (create-sequence-transformer expr-transformer-expr
                                    clause-transformer-expr))]))

(define (stream? v)
  (or (list? v)
      #; (stream-via-prop? v)  ; xxx
      ))

(define (unsafe-stream-not-empty? v)
  (if (null? v)
      #f
      (or (pair? v)
          (not ((vector-ref (stream-ref v) 0) v)))))

(define (stream-empty? v)
  (or (null? v)
      (if (stream? v)
          (if (pair? v)
              #f
              ((vector-ref (stream-ref v) 0) v))
          (error 'stream-empty?
                                "stream?"
                                v))))

(define (unsafe-stream-first v)
  (cond [(pair? v) (car v)]
        [else ((vector-ref (stream-ref v) 1) v)]))

(define (stream-first v)
  (if (and (stream? v)
           (not (stream-empty? v)))
      (unsafe-stream-first v)
      (error 'stream-first
                            "(and/c stream? (not/c stream-empty?))"
                            v)))

(define (unsafe-stream-rest v)
  (cond [(pair? v) (cdr v)]
        [else (let ([r ((vector-ref (stream-ref v) 2) v)])
                (unless (stream? r)
                  (raise-mismatch-error 'stream-rest-guard
                                        "result is not a stream: "
                                        r))
                r)]))

(define (stream-rest v)
  (if (and (stream? v)
           (not (stream-empty? v)))
      (unsafe-stream-rest v)
      (error 'stream-rest
                            "(and/c stream? (not/c stream-empty?))"
                            v)))

(define (sequence? v)
  (or (exact-nonnegative-integer? v)
      (do-sequence? v)
      ; (sequence-via-prop? v)
      (stream? v)
      ; (mpair? v)
      (vector? v)
      ;(flvector? v)
      ;(fxvector? v)
      (string? v)
      ;(bytes? v)
      ;(input-port? v)
      (range? v) ; XXX
      (hash? v)
      ; (and (:sequence? v) (not (struct-type? v))
           ))

(define (make-sequence who v)
  (cond
    [(exact-nonnegative-integer? v) (:integer-gen v)]
    [(do-sequence? v) ((do-sequence-ref v 0))]
    ; [(mpair? v) (:mlist-gen v)]
    [(list? v) (:list-gen v)]
    [(vector? v) (:vector-gen v 0 (vector-length v) 1)]
    ;[(flvector? v) (:flvector-gen v 0 (flvector-length v) 1)]
    ;[(fxvector? v) (:fxvector-gen v 0 (fxvector-length v) 1)]
    [(string? v) (:string-gen v 0 (string-length v) 1)]
    ;[(bytes? v) (:bytes-gen v 0 (bytes-length v) 1)]
    ;[(input-port? v) (:input-port-gen v)]
    ;[(hash? v) (:hash-key+val-gen v)]
    ;[(sequence-via-prop? v) ((sequence-ref v) v)]
    ;[(:sequence? v) (make-sequence who ((:sequence-ref v) v))]
    [(stream? v) (:stream-gen v)]
    [(range? v)  (:range-gen v)]
    [else (error 'make-sequence
                 (format "for: expected a sequence for ~a, got something else: ~v"
                    (if (= 1 (length who))
                        (car who)
                        who)
                    v))
          #;(raise
           (exn:fail:contract
            (format "for: expected a sequence for ~a, got something else: ~v"
                    (if (= 1 (length who))
                        (car who)
                        who)
                    v)
            (current-continuation-marks)))]))


; (make-range n add1 #f)
(struct range (start inc cont?))
(define make-range range)
#;(define-values (struct:range
                make-range
                range?
                range-ref
                range-set!)
  (make-struct-type 'stream #f 3 0 #f
                    (list (cons prop:stream
                                (vector
                                 (lambda (v)
                                   (let ([cont? (range-ref v 2)])
                                     (and cont?
                                          (not (cont? (range-ref v 0))))))
                                 (lambda (v) (range-ref v 0))
                                 (lambda (v) (make-range
                                              ((range-ref v 1) (range-ref v 0))
                                              (range-ref v 1)
                                              (range-ref v 2)))))
                          (cons prop:gen-sequence
                                (lambda (v)
                                  (values
                                   values
                                   (range-ref v 1)
                                   (range-ref v 0)
                                   (range-ref v 2)
                                   #f
                                   #f))))))

(define in-range
  (case-lambda
    [(b) (in-range 0 b 1)]
    [(a b) (in-range a b 1)]
    [(a b step)
     (unless (real? a) (error 'in-range "real?" a))
     (unless (real? b) (error 'in-range "real?" b))
     (unless (real? step) (error 'in-range "real?" step))
     (let* ([cont? (if (step . >= . 0)
                       (lambda (x) (< x b))
                       (lambda (x) (> x b)))]
            [inc (lambda (x) (+ x step))])
       ;(:range-gen a b step)
       (make-range a inc cont?))]))

(define (:range-gen a b step)
  (values values add1 a (lambda (i) (i . < . b)) #f #f))

(define (:integer-gen v)
  (values values add1 0 (lambda (i) (i . < . v)) #f #f))

(define in-naturals
  (case-lambda
    [() (in-naturals 0)]
    [(n)
     (unless (and (integer? n)
                  (exact? n)
                  (n . >= . 0))
       (error 'in-naturals
                             "exact-nonnegative-integer?"
                             n))
     (make-range n add1 #f)]))

;; [Whalesong] Implement list-stream using standard structs
(struct list-stream (empty? first rest))
(define make-list-stream list-stream)
(define (list-stream-ref ls i)
  (cond [(= i 0) (list-stream-empty? ls)]
        [(= i 1) (list-stream-first ls)]
        [(= i 2) (list-stream-rest ls)]
        [else (error 'list-stream-ref)]))

#;(define-values (struct:list-stream
                make-list-stream
                list-stream?
                list-stream-ref
                list-stream-set!)
  (make-struct-type 'stream #f 1 0 #f
                    (list (cons prop:stream
                                (vector
                                 (lambda (v) (not (pair? (list-stream-ref v 0))))
                                 (lambda (v) (car (list-stream-ref v 0)))
                                 (lambda (v) (make-list-stream (cdr (list-stream-ref v 0))))))
                          (cons prop:gen-sequence
                                (lambda (v)
                                  (values
                                   car
                                   cdr
                                   (list-stream-ref v 0)
                                   pair?
                                   #f
                                   #f))))))


(define (in-list l)
  (unless (list? l) (error 'in-list "list?" l))
  (make-list-stream l))

(define (:list-gen l)
  (values car cdr l pair? #f #f))

#;(define (in-mlist l)
  (make-do-sequence (lambda () (:mlist-gen l))))

#;(define (:mlist-gen l)
  (values mcar mcdr l mpair? #f #f))

#;(define (in-input-port-bytes p)
  (unless (input-port? p)
    (raise-argument-error 'in-input-port-bytes "input-port?" p))
  (make-do-sequence (lambda () (:input-port-gen p))))

#;(define (:input-port-gen p)
  (values read-byte values p #f
          (lambda (x) (not (eof-object? x)))
          #f))

#;(define (in-input-port-chars p)
  (unless (input-port? p)
    (raise-argument-error 'in-input-port-chars "input-port?" p))
  (in-producer (lambda () (read-char p)) eof))

#;(define (check-in-port r p)
  (unless (and (procedure? r) (procedure-arity-includes? r 1))
    (raise-argument-error 'in-port "(procedure-arity-includes/c 1)" r))
  (unless (input-port? p) (raise-argument-error 'in-port "input-port?" p)))

#;(define in-port
  (case-lambda
    [()  (in-port read (current-input-port))]
    [(r) (in-port r (current-input-port))]
    [(r p)
     (check-in-port r p)
     (in-producer (lambda () (r p)) eof)]))

#;(define (check-in-lines p mode)
  (unless (input-port? p) (raise-argument-error 'in-lines "input-port?" p))
  (unless (memq mode '(linefeed return return-linefeed any any-one))
    (raise-argument-error
     'in-lines
     "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
     mode)))

#;(define in-lines
  (case-lambda
    [()  (in-lines (current-input-port) 'any)]
    [(p) (in-lines p 'any)]
    [(p mode)
     (check-in-lines p mode)
     (in-producer (lambda () (read-line p mode)) eof)]))

#;(define (check-in-bytes-lines p mode)
  (unless (input-port? p) (raise-argument-error 'in-bytes-lines "input-port" p))
  (unless (memq mode '(linefeed return return-linefeed any any-one))
    (raise-argument-error
     'in-bytes-lines
     "(or/c 'linefeed 'return 'return-linefeed 'any 'any-one)"
     mode)))

#;(define in-bytes-lines
  (case-lambda
    [()  (in-bytes-lines (current-input-port) 'any)]
    [(p) (in-bytes-lines p 'any)]
    [(p mode)
     (check-in-bytes-lines p mode)
     (in-producer (lambda () (read-bytes-line p mode)) eof)]))

#;(define (in-hash ht)
  (unless (hash? ht) (error 'in-hash "hash?" ht))
  (make-do-sequence (lambda () (:hash-key+val-gen ht))))

#;(define (:hash-key+val-gen ht)
  (:hash-gen ht (lambda (ht pos)
                  (values (hash-iterate-key ht pos)
                          (hash-iterate-value ht pos)))))

#;(define (in-hash-keys ht)
  (unless (hash? ht) (raise-argument-error 'in-hash-keys "hash?" ht))
  (make-do-sequence (lambda () (:hash-gen ht hash-iterate-key))))
#;(define (in-hash-values ht)
  (unless (hash? ht) (raise-argument-error 'in-hash-values "hash?" ht))
  (make-do-sequence (lambda () (:hash-gen ht hash-iterate-value))))
#;(define (in-hash-pairs ht)
  (unless (hash? ht) (raise-argument-error 'in-hash-values "hash?" ht))
  (make-do-sequence (lambda ()
                      (:hash-gen ht (lambda (ht pos)
                                      (cons (hash-iterate-key ht pos)
                                            (hash-iterate-value ht pos)))))))

#;(define (:hash-gen ht sel)
  (values (lambda (pos) (sel ht pos))
          (lambda (pos) (hash-iterate-next ht pos))
          (hash-iterate-first ht)
          (lambda (pos) pos) ; #f position means stop
          #f
          #f))

(define (in-stream l)
  (unless (stream? l) (error 'in-stream "stream?" l))
  (make-do-sequence (lambda () (:stream-gen l))))

(define (:stream-gen l)
  (values unsafe-stream-first unsafe-stream-rest l unsafe-stream-not-empty? #f #f))

;; Vector-like sequences --------------------------------------------------

;; (: check-ranges (Symbol Natural Integer Integer Natural -> Void))
;;
;; As no object can have more slots than can be indexed by
;; the largest fixnum, after running these checks start,
;; stop, and step are guaranteed to be fixnums.
(define raise-range-error error)
(define raise-argument-error error)
(define raise-arguments-error error)
(define (check-ranges who vec start stop step len)
  (unless (and (exact-nonnegative-integer? start) (<= start len))
    (raise-range-error who "vector" "starting " start vec 0 len))
  (unless (and (exact-integer? stop) (<= -1 stop) (<= stop len))
    (raise-range-error who "vector" "stopping " stop vec -1 len))
  (unless (and (exact-integer? step) (not (zero? step)))
    (raise-argument-error who "(and/c exact-integer? (not/c zero?))" step))
  (when (and (< start stop) (< step 0))
    (raise-arguments-error who 
                           "starting index less then stopping index, but given a negative step"
                           "starting index" start
                           "stopping index" stop
                           "step" step))
  (when (and (< stop start) (> step 0))
    (raise-arguments-error who 
                           "starting index more then stopping index, but given a positive step"
                           "starting index" start
                           "stopping index" stop
                           "step" step)))

;; (: normalise-inputs (A) (Symbol String (Any -> Boolean) (A -> Natural) Any Any Any Any -> (values Fixnum Fixnum Fixnum)))
;;
;; Checks all inputs are valid for an in-vector sequence,
;; and if so returns the vector, start, stop, and
;; step. Start, stop, and step are guaranteed to be Fixnum
(define (normalise-inputs who type-name vector? vector-length
                          vec start stop step)
  (unless (vector? vec)
    (raise-argument-error who type-name vec))
  (let* ([len (vector-length vec)]
         [stop* (if stop stop len)])
    (check-ranges who vec start stop* step len)
    (values vec start stop* step)))

(define-syntax define-in-vector-like
  (syntax-rules ()
    [(define-in-vector-like in-vector-name
       type-name-str vector?-id vector-length-id :vector-gen-id)
     (define in-vector-name
       (case-lambda
         [(v) (in-vector-name v 0 #f 1)]
         [(v start) (in-vector-name v start #f 1)]
         [(v start stop) (in-vector-name v start stop 1)]
         [(v start stop step)
          (let-values (([v start stop step]
                        (normalise-inputs 'in-vector-name type-name-str vector?-id vector-length-id
                                          v start stop step)))
            (make-do-sequence (lambda () (:vector-gen-id v start stop step))))]))]))

(define-syntax define-:vector-like-gen
  (syntax-rules ()
    [(define-:vector-like-gen :vector-like-name vector-ref-id)
     (define (:vector-like-name v start stop step)
       (values
        ;; pos->element
        (lambda (i) (vector-ref-id v i))
        ;; next-pos
        ;; Minor optimisation.  I assume add1 is faster than \x.x+1
        (if (= step 1) add1 (lambda (i) (+ i step)))
        ;; initial pos
        start
        ;; continue?
        (if (> step 0)
            (lambda (i) (< i stop))
            (lambda (i) (> i stop)))
        #f
        #f))]))

(define-for-syntax (make-in-vector-like in-vector-name
                                        type-name-str
                                        vector?-id
                                        unsafe-vector-length-id
                                        in-vector-id
                                        unsafe-vector-ref-id)
  (define (in-vector-like stx)
    (with-syntax ([in-vector-name in-vector-name]
                  [type-name type-name-str]
                  [vector? vector?-id]
                  [in-vector in-vector-id]
                  [unsafe-vector-length unsafe-vector-length-id]
                  [unsafe-vector-ref unsafe-vector-ref-id])
      (syntax-case stx ()
        ;; Fast case
        [[(id) (_ vec-expr)]
         (for-clause-syntax-protect
          #'[(id)
             (:do-in
              ;;outer bindings
              ([(vec len) (let ([vec vec-expr])
                            (unless (vector? vec)
                              (in-vector vec))
                            (values vec (unsafe-vector-length vec)))])
              ;; outer check
              #f
              ;; loop bindings
              ([pos 0])
              ;; pos check
              (pos . < . len)
              ;; inner bindings
              ([(id) (unsafe-vector-ref vec pos)])
              ;; pre guard
              #t
              ;; post guard
              #t
              ;; loop args
              ((+ 1 pos)))])]
        ;; General case
        [((id) (_ vec-expr start))
         (in-vector-like (syntax ((id) (_ vec-expr start #f 1))))]
        [((id) (_ vec-expr start stop))
         (in-vector-like (syntax ((id) (_ vec-expr start stop 1))))]
        [((id) (_ vec-expr start stop step))
         (let ([all-fx? (memq (syntax-e #'step) '(1 -1))])
           (for-clause-syntax-protect
            #`[(id)
               (:do-in
                ;; Outer bindings
                ;; start*, stop*, and step* are guaranteed to be exact integers
                ([(v* start* stop* step*)
                  (normalise-inputs (quote in-vector-name) type-name
                                    ;; reverse-eta triggers JIT inlining of
                                    ;; primitives, which is good for futures:
                                    (lambda (x) (vector? x))
                                    (lambda (x) (unsafe-vector-length x))
                                    vec-expr start stop step)])
                ;; Outer check is done by normalise-inputs
                #t
                ;; Loop bindings
                ([idx start*])
                ;; Pos guard
                #,(cond
                    [(not (number? (syntax-e #'step)))
                     #`(if (step* . >= . 0) (< idx stop*) (> idx stop*))]
                    [((syntax-e #'step) . >= . 0)
                     (if all-fx?
                         #'(< idx stop*)
                         #'(< idx stop*))]
                    [else
                     (if all-fx?
                         #'(> idx stop*)
                         #'(> idx stop*))])
                ;; Inner bindings
                ([(id) (unsafe-vector-ref v* idx)])
                ;; Pre guard
                #t
                ;; Post guard
                #t
                ;; Loop args
                ((#,(if all-fx? #'+ #'+) idx step)))]))]
        [_ #f])))
  in-vector-like)

(define-:vector-like-gen :vector-gen vector-ref)

(define-in-vector-like in-vector
  "vector" vector? vector-length :vector-gen)

(define-sequence-syntax *in-vector
  (lambda () #'in-vector)
  (make-in-vector-like 'in-vector
                       "vector"
                       #'vector?
                       #'vector-length
                       #'in-vector
                       #'vector-ref))

(define-:vector-like-gen :string-gen string-ref)

(define-in-vector-like in-string
  "string" string? string-length :string-gen)

(define-sequence-syntax *in-string
  (lambda () #'in-string)
  (make-in-vector-like 'in-string
                       "string"
                       #'string?
                       #'string-length
                       #'in-string
                       #'string-ref))

; (define-:vector-like-gen :bytes-gen bytes-ref)

#;(define-in-vector-like in-bytes
  "bytes" bytes? bytes-length :bytes-gen)

#;(define-sequence-syntax *in-bytes
  (lambda () #'in-bytes)
  (make-in-vector-like 'in-bytes
                       "bytes"
                       #'bytes?
                       #'bytes-length
                       #'in-bytes
                       #'bytes-ref))

;(define-:vector-like-gen :flvector-gen unsafe-flvector-ref)
;; in-flvector is defined in racket/flonum
;(define-:vector-like-gen :fxvector-gen unsafe-fxvector-ref)
;; in-fxvector is defined in racket/fixnum

;; ------------------------------------------------------------------------

(define (stop-before g pred)
  (unless (sequence? g) (raise-argument-error 'stop-before "sequence?" g))
  (unless (and (procedure? pred)
               (procedure-arity-includes? pred 1))
    (raise-argument-error 'stop-before "(procedure-arity-includes/c 1)" pred))
  (make-do-sequence (lambda ()
                      (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                    (make-sequence #f g)])
                        (values pos->val
                                pos-next
                                init
                                pos-cont?
                                (case-lambda
                                  [(val) (and (if pre-cont? (pre-cont? val) #t)
                                              (not (pred val)))]
                                  [vals (and (if pre-cont? (apply pre-cont? vals) #t)
                                             (not (apply pred vals)))])
                                post-cont?)))))

(define (stop-after g pred)
  (unless (sequence? g) (raise-argument-error 'stop-after "sequence?" g))
  (unless (and (procedure? pred)
               (procedure-arity-includes? pred 1))
    (raise-argument-error 'stop-after "(procedure-arity-includes/c 1)" pred))
  (make-do-sequence (lambda ()
                      (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                    (make-sequence #f g)])
                        (values pos->val
                                pos-next
                                init
                                pos-cont?
                                pre-cont?
                                (case-lambda
                                  [(pos val) (and (if post-cont? (post-cont? pos val) #t)
                                                  (not (pred val)))]
                                  [(pos . vals) (and (if post-cont? (apply post-cont? pos vals) #t)
                                                     (not (apply pred vals)))]))))))

(define (in-indexed g)
  (unless (sequence? g) (raise-argument-error 'in-indexed "sequence?" g))
  (make-do-sequence (lambda ()
                      (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                    (make-sequence #f g)])
                        (values (lambda (pos) (values (pos->val (car pos)) (cdr pos)))
                                (lambda (pos) (cons (pos-next (car pos)) (add1 (cdr pos))))
                                (cons init 0)
                                (and pos-cont?
                                     (lambda (pos) (pos-cont? (car pos))))
                                (and pre-cont?
                                     (lambda (val idx) (pre-cont? val)))
                                (and post-cont?
                                     (lambda (pos val idx) (post-cont? pos val))))))))

(define (in-value v)
  (make-do-sequence (lambda ()
                      (values (lambda (pos) v)
                              (lambda (pos) #f)
                              #t
                              (lambda (pos) pos)
                              void
                              void))))

(define (in-values-sequence g)
  (unless (sequence? g) (raise-argument-error 'in-values-sequence "sequence?" g))
  (make-do-sequence (lambda ()
                      (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                    (make-sequence #f g)])
                        (values (lambda (pos) (call-with-values (lambda () (pos->val pos))
                                                                list))
                                pos-next
                                init
                                pos-cont?
                                (and pre-cont?
                                     (lambda (vals) (apply pre-cont? vals)))
                                (and post-cont?
                                     (lambda (pos vals) (apply post-cont? pos vals))))))))

(define (in-values*-sequence g)
  (unless (sequence? g) (raise-argument-error 'in-values-sequence "sequence?" g))
  (make-do-sequence (lambda ()
                      (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                                    (make-sequence #f g)])
                        (values (lambda (pos) (call-with-values (lambda () (pos->val pos))
                                                                (case-lambda
                                                                  [(v) (if (list? v) (list v) v)]
                                                                  [vs vs])))
                                pos-next
                                init
                                pos-cont?
                                (and pre-cont?
                                     (lambda (vals)
                                       (if (list? vals)
                                           (apply pre-cont? vals)
                                           (pre-cont? vals))))
                                (and post-cont?
                                     (lambda (pos vals)
                                       (if (list? vals)
                                           (apply post-cont? pos vals)
                                           (post-cont? pos vals)))))))))

;; ----------------------------------------

(define (append-sequences sequences cyclic?)
  (define (seqs->m+g+r seqs)
    (if (pair? seqs)
        (let-values ([(more? get) (sequence-generate (car seqs))]
                     [(seqs) (cdr seqs)])
          (if (more?) (list* more? get seqs) (seqs->m+g+r seqs)))
        (and cyclic? (seqs->m+g+r sequences))))
  (make-do-sequence
   (lambda ()
     ;; place is (cur-more? cur-get rest-seqs ...) or #f
     (values (lambda (m+g+r) ((cadr m+g+r)))
             (lambda (m+g+r)
               (if (and (pair? m+g+r) (not ((car m+g+r))))
                   (seqs->m+g+r (cddr m+g+r))
                   m+g+r))
             (seqs->m+g+r sequences)
             values
             void
             void))))

(define (check-sequences who sequences)
  (for-each (lambda (g)
              (unless (sequence? g) (raise-argument-error who "sequence?" g)))
            sequences))

(define (in-sequences . sequences)
  (check-sequences 'in-sequences sequences)
  (if (and (pair? sequences) (null? (cdr sequences)))
      (car sequences)
      (append-sequences sequences #f)))
(define (in-cycle . sequences)
  (check-sequences 'in-cycle sequences)
  (append-sequences sequences #t))

(define (in-parallel . sequences)
  (check-sequences 'in-parallel sequences)
  (if (= 1 (length sequences))
      (car sequences)
      (make-do-sequence
       (lambda ()
         (let-values ([(pos->vals pos-nexts inits pos-cont?s pre-cont?s post-cont?s)
                       (for/lists (p->v p-s i ps? pr? po?) ([g sequences])
                         (make-sequence #f g))])
           (values
            (lambda (poses) (apply values (map (lambda (pos->val pos) (pos->val pos))
                                               pos->vals
                                               poses)))
            (lambda (poses) (map (lambda (pos-next pos) (pos-next pos))
                                 pos-nexts
                                 poses))
            inits
            (and (ormap values pos-cont?s)
                 (lambda (poses) (andmap (lambda (pos-cont? pos)
                                           (if pos-cont? (pos-cont? pos) #t))
                                         pos-cont?s
                                         poses)))
            (and (ormap values pre-cont?s)
                 (lambda vals (andmap (lambda (pre-cont? val)
                                        (if pre-cont? (pre-cont? val) #t))
                                      pre-cont?s
                                      vals)))
            (and (ormap values post-cont?s)
                 (lambda (poses . vals) (andmap (lambda (post-cont? pos val)
                                                  (if post-cont? (post-cont? pos val) #t))
                                                post-cont?s
                                                poses
                                                vals)))))))))

(define in-producer
  (case-lambda
    [(producer)
     ;; simple stop-less version
     (make-do-sequence (lambda () (values (λ _ (producer)) void (void) #f #f #f)))]
    [(producer stop . more)
     (define produce!
       (if (null? more)
           (lambda (_) (producer))
           (lambda (_) (apply producer more))))
     (define stop?
       (cond [(not (procedure? stop))
              (lambda (x) (not (eq? x stop)))]
             [(equal? 1 (procedure-arity stop))
              (lambda (x) (not (stop x)))]
             [else
              (lambda xs (not (apply stop xs)))]))
     (make-do-sequence
      (lambda ()
        (values produce! void (void) #f stop? #f)))]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  running sequences outside of a loop:

#;(define-values (struct:do-stream
                make-do-stream
                do-stream?
                do-stream-ref
                do-stream-set!)
  (make-struct-type 'stream #f 3 0 #f
                    (list (cons prop:stream
                                (vector
                                 (lambda (v) ((do-stream-ref v 0)))
                                 (lambda (v) ((do-stream-ref v 1)))
                                 (lambda (v) ((do-stream-ref v 2))))))))

;(define empty-stream (make-do-stream (lambda () #t) void void))

#;(define (sequence->stream s)
  (unless (sequence? s)
    (raise-argument-error 'sequence-generate "sequence?" s))
  (cond
    [(stream? s) s]
    [else
     (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                   (make-sequence #f s)])
       (define (gen-stream pos)
         (let ([done? #f]
               [vals #f]
               [empty? #f]
               [next #f])
           (define (force!)
             (unless done?
               (if (if pos-cont? (pos-cont? pos) #t)
                   (begin
                     (set! vals (call-with-values (lambda () (pos->val pos)) list))
                     (unless (if pre-cont? (apply pre-cont? vals) #t)
                       (set! vals #f)
                       (set! empty? #t)))
                   (set! empty? #t))
               (set! done? #t)))
           (make-do-stream (lambda () (force!) empty?)
                           (lambda () (force!) (apply values vals))
                           (lambda ()
                             (force!)
                             (if next
                                 next
                                 (begin
                                   (if (if post-cont? (apply post-cont? pos vals) #t)
                                       (set! next (gen-stream (pos-next pos)))
                                       (set! next empty-stream))
                                   next))))))
       (gen-stream init))]))

(define (no-more)
  (error "sequence has no more values")
  #;(raise (exn:fail:contract "sequence has no more values"
                            (current-continuation-marks))))

(define (sequence-generate g)
  (unless (sequence? g)
    (raise-argument-error 'sequence-generate "sequence?" g))
  (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                (make-sequence #f g)])
    (let ([pos init])
      (letrec ([more? #f]
               [prep-val! #f]
               [next #f])
        (letrec ([init-more?
                  (lambda () (prep-val!) (more?))]
                 [init-next
                  (lambda () (prep-val!) (next))]
                 [init-prep-val!
                  (lambda ()
                    (if (if pos-cont? (pos-cont? pos) #t)
                        (call-with-values
                         (lambda () (pos->val pos))
                         (lambda vals
                           (if (if pre-cont? (apply pre-cont? vals) #t)
                               (begin
                                 (set! more? (lambda () #t))
                                 (set! next
                                       (lambda ()
                                         (let ([v vals])
                                           (set! prep-val!
                                                 (lambda ()
                                                   (if (if post-cont?
                                                           (apply post-cont? pos vals)
                                                           #t)
                                                       (begin
                                                         (set! pos (pos-next pos))
                                                         (set! prep-val! init-prep-val!)
                                                         (prep-val!))
                                                       (begin
                                                         (set! more? (lambda () #f))
                                                         (set! next no-more)))))
                                           (set! more? init-more?)
                                           (set! next init-next)
                                           (apply values v))))
                                 (set! prep-val! void)
                                 (apply values vals))
                               (begin
                                 (set! more? (lambda () #f))
                                 (set! next no-more)))))
                        (begin
                          (set! more? (lambda () #f))
                          (set! next no-more))))])
          (set! more? init-more?)
          (set! prep-val! init-prep-val!)
          (set! next init-next)
          (let ([sequence-more? (lambda () (more?))]
                [sequence-next (lambda () (next))])
            (values sequence-more?
                    sequence-next)))))))

(define (sequence-generate* g)
  (unless (sequence? g)
    (raise-argument-error 'sequence-generate* "sequence?" g))
  (let-values ([(pos->val pos-next init pos-cont? pre-cont? post-cont?)
                (make-sequence #f g)])
    (letrec ([next!
              (lambda (pos)
                (if (if pos-cont? (pos-cont? pos) #t)
                    (call-with-values
                     (lambda () (pos->val pos))
                     (lambda vals
                       (if (if pre-cont? (apply pre-cont? vals) #t)
                           (values vals
                                   (lambda ()
                                     (if (if post-cont?
                                             (apply post-cont? pos vals)
                                             #t)
                                         (next! (pos-next pos))
                                         (values #f no-more))))
                           (values #f no-more))))
                    (values #f no-more)))])
      (next! init))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  core `for/fold' syntax

(define-syntax values*
  (syntax-rules ()
    [(_ x) x]
    [(_ x ...) (values x ...)]))

(define-syntax (for/foldX/derived stx)
  (syntax-case stx ()
    ;; Done case (no more clauses, and no generated clauses to emit):
    [(_ [orig-stx nested? emit? ()] ([fold-var fold-init] ...) ()
        expr1 expr ...)
     #`(let ([fold-var fold-init] ...) (let () expr1 expr ...))]
    ;; Switch-to-emit case (no more clauses to generate):
    [(_ [orig-stx nested? #f binds] ([fold-var fold-init] ...) () . body)
     #`(for/foldX/derived [orig-stx nested? #t binds]
                          ([fold-var fold-init] ...) () . body)]
    ;; Emit case:
    [(_ [orig-stx nested? #t binds] ([fold-var fold-init] ...) rest expr1 . body)
     (with-syntax ([(([outer-binding ...]
                      outer-check
                      [loop-binding ...]
                      pos-guard
                      [inner-binding ...]
                      pre-guard
                      post-guard
                      [loop-arg ...]) ...) (reverse (syntax->list #'binds))])
       (quasisyntax/loc #'orig-stx
         (let-values (outer-binding ... ...)
           outer-check ...
           #,(syntax/loc #'orig-stx
               (let for-loop ([fold-var fold-init] ...
                              loop-binding ... ...)
                 (if (and pos-guard ...)
                     (let-values (inner-binding ... ...)
                       (if (and pre-guard ...)
                           (let-values ([(fold-var ...)
                                         (for/foldX/derived [orig-stx nested? #f ()]
                                                            ([fold-var fold-var] ...)
                                                            rest expr1 . body)])
                             (if (and post-guard ...)
                                 (for-loop fold-var ... loop-arg ... ...)
                                 (values* fold-var ...)))
                           (values* fold-var ...)))
                     (values* fold-var ...)))))))]
    ;; Bad body cases:
    [(_ [orig-stx . _] fold-bind ())
     (raise-syntax-error
      #f "missing body expression after sequence bindings" #'orig-stx)]
    [(_ [orig-stx . _] fold-bind () . rest)
     (raise-syntax-error
      #f "bad syntax (illegal use of `.') after sequence bindings" #'orig-stx)]
    ;; Guard case, no pending emits:
    [(_ [orig-stx nested? #f ()] ([fold-var fold-init] ...) (#:when expr . rest) . body)
     #'(let ([fold-var fold-init] ...)
         (if expr
             (for/foldX/derived [orig-stx nested? #f ()]
                                ([fold-var fold-var] ...) rest . body)
             (values* fold-var ...)))]
    ;; Guard case, pending emits need to be flushed first
    [(frm [orig-stx nested? #f binds] ([fold-var fold-init] ...)
          (#:when expr . rest) . body)
     #'(frm [orig-stx nested? #t binds] ([fold-var fold-init] ...)
            (#:when expr . rest) . body)]
    ;; Negative guard case, no pending emits:
    [(_ [orig-stx nested? #f ()] ([fold-var fold-init] ...) (#:unless expr . rest) . body)
     #'(let ([fold-var fold-init] ...)
         (if expr
             (values* fold-var ...)
             (for/foldX/derived [orig-stx nested? #f ()]
                                ([fold-var fold-var] ...) rest . body)))]
    ;; Negative guard case, pending emits need to be flushed first
    [(frm [orig-stx nested? #f binds] ([fold-var fold-init] ...)
          (#:unless expr . rest) . body)
     #'(frm [orig-stx nested? #t binds] ([fold-var fold-init] ...)
            (#:unless expr . rest) . body)]
    ;; Convert single-value form to multi-value form:
    [(_ [orig-stx nested? #f binds] fold-bind ([id rhs] . rest) . body)
     (identifier? #'id)
     #'(for/foldX/derived [orig-stx nested? #f binds] fold-bind
                          ([(id) rhs] . rest) . body)]
    ;; If we get here in single-value mode, then it's a bad clause:
    [(_ [orig-stx #f #f nested? #f binds] fold-bind (clause . rest) . body)
     (raise-syntax-error
      #f "bad sequence binding clause" #'orig-stx #'clause)]
    ;; Expand one multi-value clause, and push it into the results to emit:
    [(frm [orig-stx nested? #f binds] ([fold-var fold-init] ...)
          (clause . rest) . body)
     (with-syntax ([bind (expand-clause #'orig-stx #'clause)])
       (let ([r #`(frm [orig-stx nested? nested? (bind . binds)]
                       ([fold-var fold-init] ...) rest . body)]
             [d (syntax-property #'bind 'disappeared-use)])
         (if d
             (syntax-property r 'disappeared-use d)
             r)))]
    [(_ [orig-stx . _] . _)
     (raise-syntax-error #f "bad syntax" #'orig-stx)]))

(define-syntax (for/foldX/derived/break stx)
  (syntax-case stx ()
    [(_ [orig-stx nested? emit? ()] ([id init] ...) (clause ...) body ...)
     (ormap (lambda (form)
              (or (eq? (syntax-e form) '#:break)
                  (eq? (syntax-e form) '#:final)))
            (syntax->list #'(clause ... body ...)))
     ;; Add an accumulator for short-circuiting
     (with-syntax ([body
                    (let loop ([bodys (syntax->list #'(body ...))] [accum null])
                      (cond
                        [(null? bodys)
                         (if (null? accum)
                             (raise-syntax-error #f "missing final body expression" #'orig-stx)
                             #`(let-values ([(id ...) (let () #,@(reverse accum))])
                                 (values stop-after? id ...)))]
                        [(or (eq? '#:break (syntax-e (car bodys)))
                             (eq? '#:final (syntax-e (car bodys))))
                         (let ([break? (eq? '#:break (syntax-e (car bodys)))])
                           (if (null? (cdr bodys))
                               (raise-syntax-error #f 
                                                   (format "missing expression after ~a" (syntax-e (car bodys)))
                                                   #'orig-stx (car bodys))
                               #`(let ()
                                   #,@(reverse accum)
                                   #,(if break?
                                         #`(if #,(cadr bodys)
                                               (values #t id ...)
                                               (let () #,(loop (cddr bodys) null)))
                                         #`(let ([stop-after? (or #,(cadr bodys) stop-after?)])
                                             #,(loop (cddr bodys) null))))))]
                        [else (loop (cdr bodys) (cons (car bodys) accum))]))]
                   [(limited-for-clause ...)
                    ;; If nested, wrap all binding clauses. Otherwise, wrap
                    ;; only the first and the first after each keyword clause:
                    (let loop ([fcs (syntax->list #'(clause ...))] [wrap? #t])
                      (cond
                        [(null? fcs) null]
                        [(eq? '#:break (syntax-e (car fcs)))
                         (when (null? (cdr fcs))
                           (raise-syntax-error #f "no expression after #:break" #'orig-stx (car fcs)))
                         (list* #'#:when #'#t
                                #`[stop? (*in-value #,(cadr fcs))]
                                #'#:when #'#t
                                #`[stop-after? (*in-value (or stop-after? stop?))]
                                #'#:unless #'stop?
                                (loop (cddr fcs) #t))]
                        [(eq? '#:final (syntax-e (car fcs)))
                         (when (null? (cdr fcs))
                           (raise-syntax-error #f "no expression after #:break" #'orig-stx (car fcs)))
                         (list* #'#:when #'#t
                                #`[stop-after? (*in-value (or #,(cadr fcs) stop-after?))]
                                #'#:when #'#t
                                (loop (cddr fcs) #t))]
                        [(keyword? (syntax-e (car fcs)))
                         (if (null? (cdr fcs))
                             fcs
                             (list* (car fcs) (cadr fcs) (loop (cddr fcs) #t)))]
                        [(not wrap?)
                         (cons (car fcs) (loop (cdr fcs) #f))]
                        [else
                         (define fc (car fcs))
                         (define wrapped-fc
                           (syntax-case fc ()
                             [[ids rhs]
                              (or (identifier? #'ids)
                                  (let ([l (syntax->list #'ids)])
                                    (and l (andmap identifier? l))))
                              (syntax/loc fc [ids (stop-after
                                                   rhs
                                                   (lambda x stop-after?))])]
                             [_ fc]))
                         (cons wrapped-fc
                               (loop (cdr fcs) (syntax-e #'nested?)))]))])
       #'(let-values ([(stop? id ...) 
                       (for/foldX/derived [orig-stx nested? emit? ()] ([stop-after? #f] [id init] ...) 
                                          (limited-for-clause ...) 
                                          body)])
           (values id ...)))]
    [(_ . rest)
     #'(for/foldX/derived . rest)]))

(define-syntax for/fold/derived
  (syntax-rules ()
    [(_ orig-stx . rest)
     (for/foldX/derived/break [orig-stx #f #f ()] . rest)]))

(define-syntax for*/fold/derived
  (syntax-rules ()
    [(_ orig-stx . rest)
     (for/foldX/derived/break [orig-stx #t #f ()] . rest)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  derived `for' syntax

(define-for-syntax (split-for-body stx body-stx)
  (let ([lst (syntax->list body-stx)])
    (if lst
        (let loop ([exprs lst] [pre-kw null] [post-kw null])
          (cond
            [(null? exprs)
             (if (null? post-kw)
                 (raise-syntax-error #f
                                     (format "missing body form after ~a clause" (syntax-e (cadr pre-kw)))
                                     stx
                                     (cadr pre-kw))
                 (list (reverse pre-kw) (reverse post-kw)))]
            [(memq (syntax-e (car exprs)) '(#:break #:final))
             (if (pair? (cdr exprs))
                 (loop (cddr exprs) 
                       (append (list* (cadr exprs) (car exprs) post-kw)
                               pre-kw)
                       null)
                 (raise-syntax-error #f
                                     (format "missing expression after ~a" (syntax-e (car exprs)))
                                     stx
                                     (car exprs)))]
            [else
             (loop (cdr exprs) pre-kw (cons (car exprs) post-kw))]))
        (raise-syntax-error #f "bad syntax" stx))))

(define-for-syntax (for-variant-stx stx derived-id-stx fold-bind-stx wrap rhs-wrap combine)
  (with-syntax ([derived-id derived-id-stx]
                [fold-bind fold-bind-stx])
    (syntax-case stx ()
      ;; When there's a bindings clause...
      [(_ (bind ...) expr1 expr ...)
       (with-syntax ([(bind ...)
                      (let loop ([bs (syntax->list #'(bind ...))])
                        (if (null? bs)
                            null
                            (syntax-case (car bs) ()
                              [[ids rhs]
                               (or (identifier? #'ids)
                                   (andmap identifier? (or (syntax->list #'ids) '(#f))))
                               (cons #`[ids #,(rhs-wrap #'rhs)]
                                     (loop (cdr bs)))]
                              [kw
                               (memq (syntax-e #'kw) '(#:when #:unless #:break #:final))
                               (cons (car bs)
                                     (if (null? (cdr bs))
                                         null
                                         (cons (cadr bs) (loop (cddr bs)))))]
                              [_
                               ;; a syntax error; let the /derived form
                               ;; handle it, and no need to wrap any more:
                               bs])))]
                     [((middle-expr ...) (end-expr ...))
                      (split-for-body stx #'(expr1 expr ...))])
         (quasisyntax/loc stx
           #,(wrap (quasisyntax/loc stx
                     (derived-id #,stx fold-bind (bind ...)
                                 middle-expr ...
                                 #,(combine (syntax/loc stx (let () end-expr ...))))))))]
      ;; Let `derived-id' complain about the missing bindings and body expression:
      [(_ . rest)
       #`(derived-id #,stx fold-bind . rest)])))

(define-syntax define-syntax-via-derived
  (syntax-rules ()
    [(_ id derived-id fold-bind wrap rhs-wrap combine)
     (define-syntax (id stx)
       (for-variant-stx stx #'derived-id #'fold-bind wrap rhs-wrap combine))]))

(define-syntax define-for-variants
  (syntax-rules ()
    [(_ (for for*) fold-bind wrap rhs-wrap combine)
     (begin
       (define-syntax-via-derived for for/fold/derived fold-bind wrap rhs-wrap combine)
       (define-syntax-via-derived for* for*/fold/derived fold-bind wrap rhs-wrap combine))]))

(define-syntax (for/fold stx)
  (syntax-case stx ()
    [(_ . rest) (quasisyntax/loc stx (for/fold/derived #,stx . rest))]))
(define-syntax (for*/fold stx)
  (syntax-case stx ()
    [(_ . rest) (quasisyntax/loc stx (for*/fold/derived #,stx . rest))]))

(define-for-variants (for for*)
  ([fold-var (void)])
  (lambda (x) x)
  (lambda (x) x)
  (lambda (x) `(,#'begin ,x ,#'(void))))

(define-for-variants (for/list for*/list)
  ([fold-var null])
  (lambda (x) `(,#'reverse ,x))
  (lambda (x) x)
  (lambda (x) `(,#'cons ,x ,#'fold-var)))

(define (vector-copy! dest dest-start src src-start [src-end (vector-length src)])
  (let loop ([s src-start] [d dest-start])
    (when (< s src-end)
      (vector-set! dest d (vector-ref src s))
      (loop (+ s 1) (+ d 1)))))

(define (grow-vector vec)
  (define n (vector-length vec))
  (define new-vec (make-vector (* 2 n)))
  (vector-copy! new-vec 0 vec 0 n)
  new-vec)

(define (shrink-vector vec i)
  (define new-vec (make-vector i))
  (vector-copy! new-vec 0 vec 0 i)
  new-vec)

(define-for-syntax (for_/vector stx orig-stx for_/vector-stx for_/fold/derived-stx wrap-all?)
  (syntax-case stx ()
    [(_ (for-clause ...) body ...)
     (with-syntax ([orig-stx orig-stx]
                   [for_/fold/derived for_/fold/derived-stx]
                   [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))])
       (syntax/loc stx
         (let-values ([(vec i)
                       (for_/fold/derived
                        orig-stx
                        ([vec (make-vector 16)]
                         [i 0])
                        (for-clause ...) 
                        middle-body ...
                        (let ([new-vec (if (eq? i (vector-length vec))
                                           (grow-vector vec)
                                           vec)])
                          (vector-set! new-vec i (let () last-body ...))
                          (values new-vec (+ i 1))))])
           (shrink-vector vec i))))]
    [(_ #:length length-expr #:fill fill-expr (for-clause ...) body ...)
     (with-syntax ([orig-stx orig-stx]
                   [(limited-for-clause ...)
                    ;; If `wrap-all?', wrap all binding clauses. Otherwise, wrap
                    ;; only the first and the first after each keyword clause:
                    (let loop ([fcs (syntax->list #'(for-clause ...))] [wrap? #t])
                      (cond
                        [(null? fcs) null]
                        [(keyword? (syntax-e (car fcs)))
                         (if (null? (cdr fcs))
                             fcs
                             (list* (car fcs) (cadr fcs) (loop (cddr fcs) #t)))]
                        [(not wrap?)
                         (cons (car fcs) (loop (cdr fcs) #f))]
                        [else
                         (define fc (car fcs))
                         (define wrapped-fc
                           (syntax-case fc ()
                             [[ids rhs]
                              (or (identifier? #'ids)
                                  (let ([l (syntax->list #'ids)])
                                    (and l (andmap identifier? l))))
                              (syntax/loc fc [ids (stop-after
                                                   rhs
                                                   (lambda x
                                                     (= i len)))])]
                             [_ fc]))
                         (cons wrapped-fc
                               (loop (cdr fcs) wrap-all?))]))]
                   [((middle-body ...) (last-body ...)) (split-for-body stx #'(body ...))]
                   [for_/vector for_/vector-stx]
                   [for_/fold/derived for_/fold/derived-stx])
       (syntax/loc stx
         (let ([len length-expr])
           (unless (exact-nonnegative-integer? len)
             (raise-argument-error 'for_/vector "exact-nonnegative-integer?" len))
           (let ([v (make-vector len fill-expr)])
             (unless (zero? len)
               (for_/fold/derived
                orig-stx 
                ([i 0])
                (limited-for-clause ...)
                middle-body ...
                (unsafe-vector*-set! v i (let () last-body ...))
                (+ 1 i)))
             v))))]
    [(_ #:length length-expr (for-clause ...) body ...)
     (for_/vector #'(fv #:length length-expr #:fill 0 (for-clause ...) body ...) 
                  orig-stx for_/vector-stx for_/fold/derived-stx wrap-all?)]))

(define-syntax (for/vector stx)
  (for_/vector stx stx #'for/vector #'for/fold/derived #f))

(define-syntax (for*/vector stx)
  (for_/vector stx stx #'for*/vector #'for*/fold/derived #t))


(define-for-syntax (do-for/lists for/fold-id stx)
  (syntax-case stx ()
    [(_ (id ...) bindings expr1 expr ...)
     (let ([ids (syntax->list #'(id ...))])
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error #f
                                         "not an identifier"
                                         stx
                                         id)))
                 ids)
       (with-syntax ([(id2 ...) (generate-temporaries ids)]
                     [for/fold for/fold-id]
                     [orig-stx stx])
         #'(let-values ([(id ...)
                         (for/fold orig-stx ([id null] ...) bindings
                           (let-values ([(id2 ...) (let ()
                                                     expr1
                                                     expr ...)])
                             (values* (cons id2 id) ...)))])
             (values* (reverse id) ...))))]))

(define-syntax (for/lists stx) (do-for/lists #'for/fold/derived stx))
(define-syntax (for*/lists stx) (do-for/lists #'for*/fold/derived stx))

(define-for-variants (for/and for*/and)
  ([result #t])
  (lambda (x) x)
  (lambda (rhs) #`(stop-after #,rhs (lambda x (not result))))
  (lambda (x) x))

(define-for-variants (for/or for*/or)
  ([result #f])
  (lambda (x) x)
  (lambda (rhs) #`(stop-after #,rhs (lambda x result)))
  (lambda (x) x))

(define-for-variants (for/first for*/first)
  ([val #f] [stop? #f])
  (lambda (x) #`(let-values ([(val _) #,x]) val))
  (lambda (rhs) #`(stop-after #,rhs (lambda x stop?)))
  (lambda (x) #`(values #,x #t)))

(define-for-variants (for/last for*/last)
  ([result #f])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x) x))

(define-for-variants (for/sum for*/sum)
  ([result 0])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x) #`(+ result #,x)))

(define-for-variants (for/product for*/product)
  ([result 1])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x) #`(* result #,x)))

(define-for-variants (for/hash for*/hash)
  ([table #hash()])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x)
    #`(let-values ([(key val) #,x])
        (hash-set table key val))))

(define-for-variants (for/hasheq for*/hasheq)
  ([table #hasheq()])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x)
    #`(let-values ([(key val) #,x])
        (hash-set table key val))))

(define-for-variants (for/hasheqv for*/hasheqv)
  ([table #hasheqv()])
  (lambda (x) x)
  (lambda (rhs) rhs)
  (lambda (x)
    #`(let-values ([(key val) #,x])
        (hash-set table key val))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  specific sequences

(define-sequence-syntax *in-range
  (lambda () #'in-range)
  (lambda (stx)
    (let loop ([stx stx])
      (syntax-case stx ()
        [[(id) (_ a b step)]
         (let ([all-fx? (and (fixnum? (syntax-e #'a))
                             (fixnum? (syntax-e #'b))
                             (memq (syntax-e #'step) '(1 -1)))])
           (for-clause-syntax-protect
            #`[(id)
               (:do-in
                ;; outer bindings:
                ([(start) a] [(end) b] [(inc) step])
                ;; outer check:
                (unless (and (real? start) (real? end) (real? inc))
                  ;; let `in-range' report the error:
                  (in-range start end inc))
                ;; loop bindings:
                ([pos start])
                ;; pos check
                #,(cond [all-fx?
                         ;; Special case, can use unsafe ops:
                         (if ((syntax-e #'step) . >= . 0)
                             #'(< pos end)
                             #'(> pos end))]
                        ;; General cases:
                        [(not (number? (syntax-e #'step)))
                         #`(if (step . >= . 0) (< pos end) (> pos end))]
                        [((syntax-e #'step) . >= . 0)
                         #'(< pos end)]
                        [else
                         #'(> pos end)])
                ;; inner bindings
                ([(id) pos])
                ;; pre guard
                #t
                ;; post guard
                #t
                ;; loop args
                ((#,(if all-fx? #'+ #'+) pos inc)))]))]
        [[(id) (_ a b)] (loop #'[(id) (_ a b 1)])]
        [[(id) (_ b)] (loop #'[(id) (_ 0 b 1)])]
        [_ #f]))))

(define-sequence-syntax *in-naturals
  (lambda () #'in-naturals)
  (lambda (stx)
    (let loop ([stx stx])
      (syntax-case stx ()
        [[(id) (_ start-expr)]
         (for-clause-syntax-protect
          #`[(id)
             (:do-in
              ;; outer bindings:
              ([(start) start-expr])
              ;; outer check:
              (unless (exact-nonnegative-integer? start)
                ;; let `in-naturals' report the error:
                (in-naturals start))
              ;; loop bindings:
              ([pos start])
              ;; pos check
              #t
              ;; inner bindings
              ([(id) pos])
              ;; pre guard
              #t
              ;; post guard
              #t
              ;; loop args
              ((+ pos 1)))])]
        [[(id) (_)]
         (loop #'[(id) (_ 0)])]
        [_ #f]))))

(define-sequence-syntax *in-list
  (lambda () #'in-list)
  (lambda (stx)
    (syntax-case stx (list)
      [[(id) (_ (list expr))] #'[(id) (:do-in ([(id) expr]) #t () #t () #t #f ())]]
      [[(id) (_ lst-expr)]
       (for-clause-syntax-protect
        #'[(id)
           (:do-in
            ;;outer bindings
            ([(lst) lst-expr])
            ;; outer check
            (unless (list? lst) (in-list lst))
            ;; loop bindings
            ([lst lst])
            ;; pos check
            (pair? lst)
            ;; inner bindings
            ([(id) (car lst)])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            ((cdr lst)))])]
      [_ #f])))

(define-sequence-syntax *in-mlist
  (lambda () #'in-mlist)
  (lambda (stx)
    (syntax-case stx (mlist)
      [[(id) (_ (mlist expr))] #'[(id) (:do-in ([(id) expr]) #t () #t () #t #f ())]]
      [[(id) (_ lst-expr)]
       (for-clause-syntax-protect
        #'[(id)
           (:do-in
            ;;outer bindings
            ([(lst) lst-expr])
            ;; outer check
            (void) ; (unless (list? lst) (in-list lst))
            ;; loop bindings
            ([lst lst])
            ;; pos check
            (not (null? lst))
            ;; inner bindings
            ([(id) (mcar lst)])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            ((mcdr lst)))])]
      [_ #f])))

(define-sequence-syntax *in-stream
  (lambda () #'in-stream)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ lst-expr)]
       (for-clause-syntax-protect
        #'[(id)
           (:do-in
            ;;outer bindings
            ([(lst) lst-expr])
            ;; outer check
            (unless (stream? lst) (in-stream lst))
            ;; loop bindings
            ([lst lst])
            ;; pos check
            (unsafe-stream-not-empty? lst)
            ;; inner bindings
            ([(id) (unsafe-stream-first lst)])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            ((unsafe-stream-rest lst)))])]
      [_ #f])))

(define-sequence-syntax *in-indexed
  (lambda () #'in-indexed)
  (lambda (stx)
    (syntax-case stx ()
      [[(id1 id2) (_ gen-expr)]
       #'[(id1 id2) (in-parallel gen-expr (*in-naturals))]])))

(define-sequence-syntax *in-value
  (lambda () #'in-value)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ expr)]
       #'[(id) (:do-in ([(id) expr]) #t () #t () #t #f ())]])))

(define-sequence-syntax *in-producer
  (lambda () #'in-producer)
  (lambda (stx)
    (syntax-case stx ()
      ;; cheap & simple stop-less and arg-less version
      [[(id ...) (_ producer)]
       #'[(id ...)
          (:do-in ([(producer*) producer]) #t () #t ([(id ...) (producer*)])
                  #t #t ())]]
      ;; full version
      [[(id ...) (_ producer stop more ...)]
       (with-syntax ([(more* ...) (generate-temporaries #'(more ...))]
                     [single? (= 1 (length (syntax->list #'(id ...))))])
         #'[(id ...)
            (:do-in
             ;; outer bindings
             ([(producer*) producer]
              [(more*) more] ...
              [(stop?)
               (let ([s stop])
                 (cond [(procedure? s) s]
                       ['single? (lambda (x) (eq? x s))]
                       [else (error 'in-producer
                                    "stop condition for ~a, got: ~e"
                                    "multiple values must be a predicate" s)]))])
             ;; outer check
             #t
             ;; loop bindings
             ()
             ;; pos check
             #t
             ;; inner bindings
             ([(id ...) (producer* more* ...)])
             ;; pre guard
             (not (stop? id ...))
             ;; post guard
             #t
             ;; loop args
             ())])])))

;; Some iterators that are implemented using `*in-producer' (note: do not use
;; `in-producer', since in this module it is the procedure version).

(define-sequence-syntax *in-port
  (lambda () #'in-port)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_)]   #'[(id) (*in-port read (current-input-port))]]
      [[(id) (_ r)] #'[(id) (*in-port r (current-input-port))]]
      [[(id) (_ r p)]
       #'[(id) (*in-producer
                (let ([r* r] [p* p])
                  (check-in-port r* p*)
                  (lambda () (r* p*)))
                eof)]])))

(define-sequence-syntax *in-lines
  (lambda () #'in-lines)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_)]   #'[(id) (*in-lines (current-input-port) 'any)]]
      [[(id) (_ p)] #'[(id) (*in-lines p 'any)]]
      [[(id) (_ p mode)]
       #'[(id) (*in-producer
                (let ([p* p] [mode* mode])
                  (check-in-lines p* mode*)
                  (lambda () (read-line p* mode*)))
                eof)]])))

(define-sequence-syntax *in-bytes-lines
  (lambda () #'in-bytes-lines)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_)]   #'[(id) (*in-bytes-lines (current-input-port) 'any)]]
      [[(id) (_ p)] #'[(id) (*in-bytes-lines p 'any)]]
      [[(id) (_ p mode)]
       #'[(id) (*in-producer
                (let ([p* p] [mode* mode])
                  (check-in-bytes-lines p* mode*)
                  (lambda () (read-bytes-line p* mode*)))
                eof)]])))

(define-sequence-syntax *in-input-port-bytes
  (lambda () #'in-input-port-bytes)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ p)]
       #'[(id) (*in-producer
                (let ([p* p])
                  (unless (input-port? p*) (in-input-port-bytes p*))
                  (lambda () (read-byte p*)))
                eof)]])))

(define-sequence-syntax *in-input-port-chars
  (lambda () #'in-input-port-chars)
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ p)]
       #'[(id) (*in-producer
                (let ([p* p])
                  (unless (input-port? p*) (in-input-port-chars p*))
                  (lambda () (read-char p*)))
                eof)]])))

#;(define (dir-list full-d d acc)
  (for/fold ([acc acc]) ([f (in-list (reverse (directory-list full-d)))])
    (cons (build-path d f) acc)))

#;(define (next-body l d init-dir use-dir?)
  (let ([full-d (path->complete-path d init-dir)])
    (if (and (directory-exists? full-d)
             (use-dir? full-d))
        (dir-list full-d d (cdr l))
        (cdr l))))

#;(define (initial-state orig-dir init-dir)
  (if orig-dir
      (dir-list (path->complete-path orig-dir init-dir)
                orig-dir null)
      (directory-list init-dir)))

#;(define *in-directory
  (case-lambda 
    [() (*in-directory #f (lambda (d) #t))]
    [(orig-dir) (*in-directory orig-dir (lambda (d) #t))]
    [(orig-dir use-dir?)
     (define init-dir (current-directory))
     ;; current state of the sequence is a list of paths to produce; when
     ;; incrementing past a directory, add the directory's immediate
     ;; content to the front of the list:
     (define (next l)
       (define d (car l))
       (next-body l d init-dir use-dir?))
     (make-do-sequence
      (lambda ()
        (values
         car
         next
         (initial-state orig-dir init-dir)
         pair?
         #f
         #f)))]))

(define-sequence-syntax in-directory
  (λ () #'*in-directory)
  (λ (stx)
    (syntax-case stx ()
      [((d) (_)) #'[(d) (*in-directory #f)]]
      [((d) (_ dir)) #'[(d) (*in-directory dir (lambda (d) #t))]]
      [((d) (_ dir use-dir?-expr))
       #'[(d) 
          (:do-in
           ([(orig-dir) (or dir #f)]
            [(init-dir) (current-directory)]
            [(use-dir?) use-dir?-expr])
           #true
           ([l (initial-state orig-dir init-dir)])
           (pair? l)
           ([(d) (car l)])
           #true
           #true
           [(next-body l d init-dir use-dir?)])]])))