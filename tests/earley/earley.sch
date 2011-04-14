(begin
   (define make-parser
     (lambda (grammar lexer)
       (letrec ((non-terminals
                 (lambda (grammar)
                   (letrec ((add-nt (lambda (nt nts) (if (member nt nts) nts (cons nt nts)))))
                     ((letrec ((def-loop
                                (lambda (defs nts)
                                  (if (pair? defs)
                                    (let ((def (car defs)))
                                      (let ((head (car def)))
                                        ((letrec ((rule-loop
                                                   (lambda (rules nts)
                                                     (if (pair? rules)
                                                       (let ((rule (car rules)))
                                                         ((letrec ((loop
                                                                    (lambda (l nts)
                                                                      (if (pair? l)
                                                                        (let ((nt (car l)))
                                                                          (loop (cdr l) (add-nt nt nts)))
                                                                        (rule-loop (cdr rules) nts)))))
                                                            loop)
                                                          rule
                                                          nts))
                                                       (def-loop (cdr defs) nts)))))
                                           rule-loop)
                                         (cdr def)
                                         (add-nt head nts))))
                                    (list->vector (reverse nts))))))
                        def-loop)
                      grammar
                      '()))))
                (ind
                 (lambda (nt nts)
                   ((letrec ((loop
                              (lambda (i)
                                (if (>= i '0) (if (equal? (vector-ref nts i) nt) i (loop (- i '1))) '#f))))
                      loop)
                    (- (vector-length nts) '1))))
                (nb-configurations
                 (lambda (grammar)
                   ((letrec ((def-loop
                              (lambda (defs nb-confs)
                                (if (pair? defs)
                                  (let ((def (car defs)))
                                    ((letrec ((rule-loop
                                               (lambda (rules nb-confs)
                                                 (if (pair? rules)
                                                   (let ((rule (car rules)))
                                                     ((letrec ((loop
                                                                (lambda (l nb-confs)
                                                                  (if (pair? l)
                                                                    (loop (cdr l) (+ nb-confs '1))
                                                                    (rule-loop (cdr rules) (+ nb-confs '1))))))
                                                        loop)
                                                      rule
                                                      nb-confs))
                                                   (def-loop (cdr defs) nb-confs)))))
                                       rule-loop)
                                     (cdr def)
                                     nb-confs))
                                  nb-confs))))
                      def-loop)
                    grammar
                    '0))))
         (let ((nts (non-terminals grammar)))
           (let ((nb-nts (vector-length nts)))
             (let ((nb-confs (+ (nb-configurations grammar) nb-nts)))
               (let ((starters (make-vector nb-nts '())))
                 (let ((enders (make-vector nb-nts '())))
                   (let ((predictors (make-vector nb-nts '())))
                     (let ((steps (make-vector nb-confs '#f)))
                       (let ((names (make-vector nb-confs '#f)))
                         (letrec ((setup-tables
                                   (lambda (grammar nts starters enders predictors steps names)
                                     (letrec ((add-conf
                                               (lambda (conf nt nts class)
                                                 (let ((i (ind nt nts)))
                                                   (vector-set! class i (cons conf (vector-ref class i)))))))
                                       (let ((nb-nts (vector-length nts)))
                                         ((letrec ((nt-loop
                                                    (lambda (i)
                                                      (if (>= i '0)
                                                        (begin
                                                          (vector-set! steps i (- i nb-nts))
                                                          (vector-set! names i (list (vector-ref nts i) '0))
                                                          (vector-set! enders i (list i))
                                                          (nt-loop (- i '1)))
                                                        '#f))))
                                            nt-loop)
                                          (- nb-nts '1))
                                         ((letrec ((def-loop
                                                    (lambda (defs conf)
                                                      (if (pair? defs)
                                                        (let ((def (car defs)))
                                                          (let ((head (car def)))
                                                            ((letrec ((rule-loop
                                                                       (lambda (rules conf rule-num)
                                                                         (if (pair? rules)
                                                                           (let ((rule (car rules)))
                                                                             (vector-set!
                                                                              names
                                                                              conf
                                                                              (list head rule-num))
                                                                             (add-conf conf head nts starters)
                                                                             ((letrec ((loop
                                                                                        (lambda (l conf)
                                                                                          (if (pair? l)
                                                                                            (let ((nt (car l)))
                                                                                              (vector-set!
                                                                                               steps
                                                                                               conf
                                                                                               (ind nt nts))
                                                                                              (add-conf
                                                                                               conf
                                                                                               nt
                                                                                               nts
                                                                                               predictors)
                                                                                              (loop
                                                                                               (cdr l)
                                                                                               (+ conf '1)))
                                                                                            (begin
                                                                                              (vector-set!
                                                                                               steps
                                                                                               conf
                                                                                               (-
                                                                                                (ind head nts)
                                                                                                nb-nts))
                                                                                              (add-conf
                                                                                               conf
                                                                                               head
                                                                                               nts
                                                                                               enders)
                                                                                              (rule-loop
                                                                                               (cdr rules)
                                                                                               (+ conf '1)
                                                                                               (+ rule-num '1)))))))
                                                                                loop)
                                                                              rule
                                                                              conf))
                                                                           (def-loop (cdr defs) conf)))))
                                                               rule-loop)
                                                             (cdr def)
                                                             conf
                                                             '1)))
                                                        '#f))))
                                            def-loop)
                                          grammar
                                          (vector-length nts)))))))
                           (setup-tables grammar nts starters enders predictors steps names)
                           (let ((parser-descr (vector lexer nts starters enders predictors steps names)))
                             (lambda (input)
                               (letrec ((ind
                                         (lambda (nt nts)
                                           ((letrec ((loop
                                                      (lambda (i)
                                                        (if (>= i '0)
                                                          (if (equal? (vector-ref nts i) nt) i (loop (- i '1)))
                                                          '#f))))
                                              loop)
                                            (- (vector-length nts) '1))))
                                        (comp-tok
                                         (lambda (tok nts)
                                           ((letrec ((loop
                                                      (lambda (l1 l2)
                                                        (if (pair? l1)
                                                          (let ((i (ind (car l1) nts)))
                                                            (if i (loop (cdr l1) (cons i l2)) (loop (cdr l1) l2)))
                                                          (cons (car tok) (reverse l2))))))
                                              loop)
                                            (cdr tok)
                                            '())))
                                        (input->tokens
                                         (lambda (input lexer nts)
                                           (list->vector (map (lambda (tok) (comp-tok tok nts)) (lexer input)))))
                                        (make-states
                                         (lambda (nb-toks nb-confs)
                                           (let ((states (make-vector (+ nb-toks '1) '#f)))
                                             ((letrec ((loop
                                                        (lambda (i)
                                                          (if (>= i '0)
                                                            (let ((v (make-vector (+ nb-confs '1) '#f)))
                                                              (vector-set! v '0 '-1)
                                                              (vector-set! states i v)
                                                              (loop (- i '1)))
                                                            states))))
                                                loop)
                                              nb-toks))))
                                        (conf-set-get (lambda (state conf) (vector-ref state (+ conf '1))))
                                        (conf-set-get*
                                         (lambda (state state-num conf)
                                           (let ((conf-set (conf-set-get state conf)))
                                             (if conf-set
                                               conf-set
                                               (let ((conf-set (make-vector (+ state-num '6) '#f)))
                                                 (vector-set! conf-set '1 '-3)
                                                 (vector-set! conf-set '2 '-1)
                                                 (vector-set! conf-set '3 '-1)
                                                 (vector-set! conf-set '4 '-1)
                                                 (vector-set! state (+ conf '1) conf-set)
                                                 conf-set)))))
                                        (conf-set-merge-new!
                                         (lambda (conf-set)
                                           (vector-set!
                                            conf-set
                                            (+ (vector-ref conf-set '1) '5)
                                            (vector-ref conf-set '4))
                                           (vector-set! conf-set '1 (vector-ref conf-set '3))
                                           (vector-set! conf-set '3 '-1)
                                           (vector-set! conf-set '4 '-1)))
                                        (conf-set-head (lambda (conf-set) (vector-ref conf-set '2)))
                                        (conf-set-next (lambda (conf-set i) (vector-ref conf-set (+ i '5))))
                                        (conf-set-member?
                                         (lambda (state conf i)
                                           (let ((conf-set (vector-ref state (+ conf '1))))
                                             (if conf-set (conf-set-next conf-set i) '#f))))
                                        (conf-set-adjoin
                                         (lambda (state conf-set conf i)
                                           (let ((tail (vector-ref conf-set '3)))
                                             (vector-set! conf-set (+ i '5) '-1)
                                             (vector-set! conf-set (+ tail '5) i)
                                             (vector-set! conf-set '3 i)
                                             (if (< tail '0)
                                               (begin
                                                 (vector-set! conf-set '0 (vector-ref state '0))
                                                 (vector-set! state '0 conf))
                                               '#f))))
                                        (conf-set-adjoin*
                                         (lambda (states state-num l i)
                                           (let ((state (vector-ref states state-num)))
                                             ((letrec ((loop
                                                        (lambda (l1)
                                                          (if (pair? l1)
                                                            (let ((conf (car l1)))
                                                              (let ((conf-set (conf-set-get* state state-num conf)))
                                                                (if (not (conf-set-next conf-set i))
                                                                  (begin
                                                                    (conf-set-adjoin state conf-set conf i)
                                                                    (loop (cdr l1)))
                                                                  (loop (cdr l1)))))
                                                            '#f))))
                                                loop)
                                              l))))
                                        (conf-set-adjoin**
                                         (lambda (states states* state-num conf i)
                                           (let ((state (vector-ref states state-num)))
                                             (if (conf-set-member? state conf i)
                                               (let ((state* (vector-ref states* state-num)))
                                                 (let ((conf-set* (conf-set-get* state* state-num conf)))
                                                   (if (not (conf-set-next conf-set* i))
                                                     (conf-set-adjoin state* conf-set* conf i)
                                                     '#f)
                                                   '#t))
                                               '#f))))
                                        (conf-set-union
                                         (lambda (state conf-set conf other-set)
                                           ((letrec ((loop
                                                      (lambda (i)
                                                        (if (>= i '0)
                                                          (if (not (conf-set-next conf-set i))
                                                            (begin
                                                              (conf-set-adjoin state conf-set conf i)
                                                              (loop (conf-set-next other-set i)))
                                                            (loop (conf-set-next other-set i)))
                                                          '#f))))
                                              loop)
                                            (conf-set-head other-set))))
                                        (forw
                                         (lambda (states state-num starters enders predictors steps nts)
                                           (letrec ((predict
                                                     (lambda (state state-num conf-set conf nt starters enders)
                                                       ((letrec ((loop1
                                                                  (lambda (l)
                                                                    (if (pair? l)
                                                                      (let ((starter (car l)))
                                                                        (let ((starter-set
                                                                               (conf-set-get*
                                                                                state
                                                                                state-num
                                                                                starter)))
                                                                          (if (not
                                                                               (conf-set-next
                                                                                starter-set
                                                                                state-num))
                                                                            (begin
                                                                              (conf-set-adjoin
                                                                               state
                                                                               starter-set
                                                                               starter
                                                                               state-num)
                                                                              (loop1 (cdr l)))
                                                                            (loop1 (cdr l)))))
                                                                      '#f))))
                                                          loop1)
                                                        (vector-ref starters nt))
                                                       ((letrec ((loop2
                                                                  (lambda (l)
                                                                    (if (pair? l)
                                                                      (let ((ender (car l)))
                                                                        (if (conf-set-member? state ender state-num)
                                                                          (let ((next (+ conf '1)))
                                                                            (let ((next-set
                                                                                   (conf-set-get*
                                                                                    state
                                                                                    state-num
                                                                                    next)))
                                                                              (conf-set-union
                                                                               state
                                                                               next-set
                                                                               next
                                                                               conf-set)
                                                                              (loop2 (cdr l))))
                                                                          (loop2 (cdr l))))
                                                                      '#f))))
                                                          loop2)
                                                        (vector-ref enders nt))))
                                                    (reduce
                                                     (lambda (states state state-num conf-set head preds)
                                                       ((letrec ((loop1
                                                                  (lambda (l)
                                                                    (if (pair? l)
                                                                      (let ((pred (car l)))
                                                                        ((letrec ((loop2
                                                                                   (lambda (i)
                                                                                     (if (>= i '0)
                                                                                       (let ((pred-set
                                                                                              (conf-set-get
                                                                                               (vector-ref states i)
                                                                                               pred)))
                                                                                         (if pred-set
                                                                                           (let ((next (+ pred '1)))
                                                                                             (let ((next-set
                                                                                                    (conf-set-get*
                                                                                                     state
                                                                                                     state-num
                                                                                                     next)))
                                                                                               (conf-set-union
                                                                                                state
                                                                                                next-set
                                                                                                next
                                                                                                pred-set)))
                                                                                           '#f)
                                                                                         (loop2
                                                                                          (conf-set-next
                                                                                           conf-set
                                                                                           i)))
                                                                                       (loop1 (cdr l))))))
                                                                           loop2)
                                                                         head))
                                                                      '#f))))
                                                          loop1)
                                                        preds))))
                                             (let ((state (vector-ref states state-num))
                                                   (nb-nts (vector-length nts)))
                                               ((letrec ((loop
                                                          (lambda ()
                                                            (let ((conf (vector-ref state '0)))
                                                              (if (>= conf '0)
                                                                (let ((step (vector-ref steps conf)))
                                                                  (let ((conf-set (vector-ref state (+ conf '1))))
                                                                    (let ((head (vector-ref conf-set '4)))
                                                                      (vector-set!
                                                                       state
                                                                       '0
                                                                       (vector-ref conf-set '0))
                                                                      (conf-set-merge-new! conf-set)
                                                                      (if (>= step '0)
                                                                        (predict
                                                                         state
                                                                         state-num
                                                                         conf-set
                                                                         conf
                                                                         step
                                                                         starters
                                                                         enders)
                                                                        (let ((preds
                                                                               (vector-ref
                                                                                predictors
                                                                                (+ step nb-nts))))
                                                                          (reduce
                                                                           states
                                                                           state
                                                                           state-num
                                                                           conf-set
                                                                           head
                                                                           preds)))
                                                                      (loop))))
                                                                '#f)))))
                                                  loop))))))
                                        (forward
                                         (lambda (starters enders predictors steps nts toks)
                                           (let ((nb-toks (vector-length toks)))
                                             (let ((nb-confs (vector-length steps)))
                                               (let ((states (make-states nb-toks nb-confs)))
                                                 (let ((goal-starters (vector-ref starters '0)))
                                                   (conf-set-adjoin* states '0 goal-starters '0)
                                                   (forw states '0 starters enders predictors steps nts)
                                                   ((letrec ((loop
                                                              (lambda (i)
                                                                (if (< i nb-toks)
                                                                  (let ((tok-nts (cdr (vector-ref toks i))))
                                                                    (conf-set-adjoin* states (+ i '1) tok-nts i)
                                                                    (forw
                                                                     states
                                                                     (+ i '1)
                                                                     starters
                                                                     enders
                                                                     predictors
                                                                     steps
                                                                     nts)
                                                                    (loop (+ i '1)))
                                                                  '#f))))
                                                      loop)
                                                    '0)
                                                   states))))))
                                        (produce
                                         (lambda (conf i j enders steps toks states states* nb-nts)
                                           (let ((prev (- conf '1)))
                                             (if (if (>= conf nb-nts) (>= (vector-ref steps prev) '0) '#f)
                                               ((letrec ((loop1
                                                          (lambda (l)
                                                            (if (pair? l)
                                                              (let ((ender (car l)))
                                                                (let ((ender-set
                                                                       (conf-set-get (vector-ref states j) ender)))
                                                                  (if ender-set
                                                                    ((letrec ((loop2
                                                                               (lambda (k)
                                                                                 (if (>= k '0)
                                                                                   (begin
                                                                                     (if (>= k i)
                                                                                       (if (conf-set-adjoin**
                                                                                            states
                                                                                            states*
                                                                                            k
                                                                                            prev
                                                                                            i)
                                                                                         (conf-set-adjoin**
                                                                                          states
                                                                                          states*
                                                                                          j
                                                                                          ender
                                                                                          k)
                                                                                         '#f)
                                                                                       '#f)
                                                                                     (loop2
                                                                                      (conf-set-next ender-set k)))
                                                                                   (loop1 (cdr l))))))
                                                                       loop2)
                                                                     (conf-set-head ender-set))
                                                                    (loop1 (cdr l)))))
                                                              '#f))))
                                                  loop1)
                                                (vector-ref enders (vector-ref steps prev)))
                                               '#f))))
                                        (back
                                         (lambda (states states* state-num enders steps nb-nts toks)
                                           (let ((state* (vector-ref states* state-num)))
                                             ((letrec ((loop1
                                                        (lambda ()
                                                          (let ((conf (vector-ref state* '0)))
                                                            (if (>= conf '0)
                                                              (let ((conf-set (vector-ref state* (+ conf '1))))
                                                                (let ((head (vector-ref conf-set '4)))
                                                                  (vector-set! state* '0 (vector-ref conf-set '0))
                                                                  (conf-set-merge-new! conf-set)
                                                                  ((letrec ((loop2
                                                                             (lambda (i)
                                                                               (if (>= i '0)
                                                                                 (begin
                                                                                   (produce
                                                                                    conf
                                                                                    i
                                                                                    state-num
                                                                                    enders
                                                                                    steps
                                                                                    toks
                                                                                    states
                                                                                    states*
                                                                                    nb-nts)
                                                                                   (loop2
                                                                                    (conf-set-next conf-set i)))
                                                                                 (loop1)))))
                                                                     loop2)
                                                                   head)))
                                                              '#f)))))
                                                loop1)))))
                                        (backward
                                         (lambda (states enders steps nts toks)
                                           (let ((nb-toks (vector-length toks)))
                                             (let ((nb-confs (vector-length steps)))
                                               (let ((nb-nts (vector-length nts)))
                                                 (let ((states* (make-states nb-toks nb-confs)))
                                                   (let ((goal-enders (vector-ref enders '0)))
                                                     ((letrec ((loop1
                                                                (lambda (l)
                                                                  (if (pair? l)
                                                                    (let ((conf (car l)))
                                                                      (conf-set-adjoin**
                                                                       states
                                                                       states*
                                                                       nb-toks
                                                                       conf
                                                                       '0)
                                                                      (loop1 (cdr l)))
                                                                    '#f))))
                                                        loop1)
                                                      goal-enders)
                                                     ((letrec ((loop2
                                                                (lambda (i)
                                                                  (if (>= i '0)
                                                                    (begin
                                                                      (back
                                                                       states
                                                                       states*
                                                                       i
                                                                       enders
                                                                       steps
                                                                       nb-nts
                                                                       toks)
                                                                      (loop2 (- i '1)))
                                                                    '#f))))
                                                        loop2)
                                                      nb-toks)
                                                     states*)))))))
                                        (parsed?
                                         (lambda (nt i j nts enders states)
                                           (let ((nt* (ind nt nts)))
                                             (if nt*
                                               (let ((nb-nts (vector-length nts)))
                                                 ((letrec ((loop
                                                            (lambda (l)
                                                              (if (pair? l)
                                                                (let ((conf (car l)))
                                                                  (if (conf-set-member?
                                                                       (vector-ref states j)
                                                                       conf
                                                                       i)
                                                                    '#t
                                                                    (loop (cdr l))))
                                                                '#f))))
                                                    loop)
                                                  (vector-ref enders nt*)))
                                               '#f))))
                                        (deriv-trees
                                         (lambda (conf i j enders steps names toks states nb-nts)
                                           (let ((name (vector-ref names conf)))
                                             (if name
                                               (if (< conf nb-nts)
                                                 (list (list name (car (vector-ref toks i))))
                                                 (list (list name)))
                                               (let ((prev (- conf '1)))
                                                 ((letrec ((loop1
                                                            (lambda (l1 l2)
                                                              (if (pair? l1)
                                                                (let ((ender (car l1)))
                                                                  (let ((ender-set
                                                                         (conf-set-get
                                                                          (vector-ref states j)
                                                                          ender)))
                                                                    (if ender-set
                                                                      ((letrec ((loop2
                                                                                 (lambda (k l2)
                                                                                   (if (>= k '0)
                                                                                     (if (if (>= k i)
                                                                                           (conf-set-member?
                                                                                            (vector-ref states k)
                                                                                            prev
                                                                                            i)
                                                                                           '#f)
                                                                                       (let ((prev-trees
                                                                                              (deriv-trees
                                                                                               prev
                                                                                               i
                                                                                               k
                                                                                               enders
                                                                                               steps
                                                                                               names
                                                                                               toks
                                                                                               states
                                                                                               nb-nts))
                                                                                             (ender-trees
                                                                                              (deriv-trees
                                                                                               ender
                                                                                               k
                                                                                               j
                                                                                               enders
                                                                                               steps
                                                                                               names
                                                                                               toks
                                                                                               states
                                                                                               nb-nts)))
                                                                                         ((letrec ((loop3
                                                                                                    (lambda (l3 l2)
                                                                                                      (if (pair? l3)
                                                                                                        (let ((ender-tree
                                                                                                               (list
                                                                                                                (car
                                                                                                                 l3))))
                                                                                                          ((letrec ((loop4
                                                                                                                     (lambda (l4
                                                                                                                              l2)
                                                                                                                       (if (pair?
                                                                                                                            l4)
                                                                                                                         (loop4
                                                                                                                          (cdr
                                                                                                                           l4)
                                                                                                                          (cons
                                                                                                                           (append
                                                                                                                            (car
                                                                                                                             l4)
                                                                                                                            ender-tree)
                                                                                                                           l2))
                                                                                                                         (loop3
                                                                                                                          (cdr
                                                                                                                           l3)
                                                                                                                          l2)))))
                                                                                                             loop4)
                                                                                                           prev-trees
                                                                                                           l2))
                                                                                                        (loop2
                                                                                                         (conf-set-next
                                                                                                          ender-set
                                                                                                          k)
                                                                                                         l2)))))
                                                                                            loop3)
                                                                                          ender-trees
                                                                                          l2))
                                                                                       (loop2
                                                                                        (conf-set-next ender-set k)
                                                                                        l2))
                                                                                     (loop1 (cdr l1) l2)))))
                                                                         loop2)
                                                                       (conf-set-head ender-set)
                                                                       l2)
                                                                      (loop1 (cdr l1) l2))))
                                                                l2))))
                                                    loop1)
                                                  (vector-ref enders (vector-ref steps prev))
                                                  '()))))))
                                        (deriv-trees*
                                         (lambda (nt i j nts enders steps names toks states)
                                           (let ((nt* (ind nt nts)))
                                             (if nt*
                                               (let ((nb-nts (vector-length nts)))
                                                 ((letrec ((loop
                                                            (lambda (l trees)
                                                              (if (pair? l)
                                                                (let ((conf (car l)))
                                                                  (if (conf-set-member?
                                                                       (vector-ref states j)
                                                                       conf
                                                                       i)
                                                                    (loop
                                                                     (cdr l)
                                                                     (append
                                                                      (deriv-trees
                                                                       conf
                                                                       i
                                                                       j
                                                                       enders
                                                                       steps
                                                                       names
                                                                       toks
                                                                       states
                                                                       nb-nts)
                                                                      trees))
                                                                    (loop (cdr l) trees)))
                                                                trees))))
                                                    loop)
                                                  (vector-ref enders nt*)
                                                  '()))
                                               '#f))))
                                        (nb-deriv-trees
                                         (lambda (conf i j enders steps toks states nb-nts)
                                           (let ((prev (- conf '1)))
                                             (if (let ((or-part (< conf nb-nts)))
                                                   (if or-part or-part (< (vector-ref steps prev) '0)))
                                               '1
                                               ((letrec ((loop1
                                                          (lambda (l n)
                                                            (if (pair? l)
                                                              (let ((ender (car l)))
                                                                (let ((ender-set
                                                                       (conf-set-get (vector-ref states j) ender)))
                                                                  (if ender-set
                                                                    ((letrec ((loop2
                                                                               (lambda (k n)
                                                                                 (if (>= k '0)
                                                                                   (if (if (>= k i)
                                                                                         (conf-set-member?
                                                                                          (vector-ref states k)
                                                                                          prev
                                                                                          i)
                                                                                         '#f)
                                                                                     (let ((nb-prev-trees
                                                                                            (nb-deriv-trees
                                                                                             prev
                                                                                             i
                                                                                             k
                                                                                             enders
                                                                                             steps
                                                                                             toks
                                                                                             states
                                                                                             nb-nts))
                                                                                           (nb-ender-trees
                                                                                            (nb-deriv-trees
                                                                                             ender
                                                                                             k
                                                                                             j
                                                                                             enders
                                                                                             steps
                                                                                             toks
                                                                                             states
                                                                                             nb-nts)))
                                                                                       (loop2
                                                                                        (conf-set-next ender-set k)
                                                                                        (+
                                                                                         n
                                                                                         (*
                                                                                          nb-prev-trees
                                                                                          nb-ender-trees))))
                                                                                     (loop2
                                                                                      (conf-set-next ender-set k)
                                                                                      n))
                                                                                   (loop1 (cdr l) n)))))
                                                                       loop2)
                                                                     (conf-set-head ender-set)
                                                                     n)
                                                                    (loop1 (cdr l) n))))
                                                              n))))
                                                  loop1)
                                                (vector-ref enders (vector-ref steps prev))
                                                '0)))))
                                        (nb-deriv-trees*
                                         (lambda (nt i j nts enders steps toks states)
                                           (let ((nt* (ind nt nts)))
                                             (if nt*
                                               (let ((nb-nts (vector-length nts)))
                                                 ((letrec ((loop
                                                            (lambda (l nb-trees)
                                                              (if (pair? l)
                                                                (let ((conf (car l)))
                                                                  (if (conf-set-member?
                                                                       (vector-ref states j)
                                                                       conf
                                                                       i)
                                                                    (loop
                                                                     (cdr l)
                                                                     (+
                                                                      (nb-deriv-trees
                                                                       conf
                                                                       i
                                                                       j
                                                                       enders
                                                                       steps
                                                                       toks
                                                                       states
                                                                       nb-nts)
                                                                      nb-trees))
                                                                    (loop (cdr l) nb-trees)))
                                                                nb-trees))))
                                                    loop)
                                                  (vector-ref enders nt*)
                                                  '0))
                                               '#f)))))
                                 (let ((lexer (vector-ref parser-descr '0)))
                                   (let ((nts (vector-ref parser-descr '1)))
                                     (let ((starters (vector-ref parser-descr '2)))
                                       (let ((enders (vector-ref parser-descr '3)))
                                         (let ((predictors (vector-ref parser-descr '4)))
                                           (let ((steps (vector-ref parser-descr '5)))
                                             (let ((names (vector-ref parser-descr '6)))
                                               (let ((toks (input->tokens input lexer nts)))
                                                 (vector
                                                  nts
                                                  starters
                                                  enders
                                                  predictors
                                                  steps
                                                  names
                                                  toks
                                                  (backward
                                                   (forward starters enders predictors steps nts toks)
                                                   enders
                                                   steps
                                                   nts
                                                   toks)
                                                  parsed?
                                                  deriv-trees*
                                                  nb-deriv-trees*))))))))))))))))))))))))
   (define parse->parsed?
     (lambda (parse nt i j)
       (let ((nts (vector-ref parse '0)))
         (let ((enders (vector-ref parse '2)))
           (let ((states (vector-ref parse '7)))
             (let ((parsed? (vector-ref parse '8))) (parsed? nt i j nts enders states)))))))
   (define parse->trees
     (lambda (parse nt i j)
       (let ((nts (vector-ref parse '0)))
         (let ((enders (vector-ref parse '2)))
           (let ((steps (vector-ref parse '4)))
             (let ((names (vector-ref parse '5)))
               (let ((toks (vector-ref parse '6)))
                 (let ((states (vector-ref parse '7)))
                   (let ((deriv-trees* (vector-ref parse '9)))
                     (deriv-trees* nt i j nts enders steps names toks states))))))))))
   (define parse->nb-trees
     (lambda (parse nt i j)
       (let ((nts (vector-ref parse '0)))
         (let ((enders (vector-ref parse '2)))
           (let ((steps (vector-ref parse '4)))
             (let ((toks (vector-ref parse '6)))
               (let ((states (vector-ref parse '7)))
                 (let ((nb-deriv-trees* (vector-ref parse '10)))
                   (nb-deriv-trees* nt i j nts enders steps toks states)))))))))
   (define test
     (lambda (k)
       (let ((p (make-parser '((s (a) (s s))) 
			     (lambda (l) 
			       (map (lambda (x) (list x x)) l)))))
         (let ((x (p (vector->list (make-vector k 'a))))) 
	   (display (length (parse->trees x 's '0 k))) 
	   (newline)))))
   (test '12))
