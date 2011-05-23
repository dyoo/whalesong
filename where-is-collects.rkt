#lang typed/racket/base
(require/typed racket/path
               (normalize-path (Path -> Path)))
(require/typed typed/racket/base
               (relative-path? (Any -> Boolean))
               (find-executable-path (Path Path -> Path)))

(provide collects-path)

(define collects-path
  (normalize-path
   (let ([p (find-system-path 'collects-dir)])
     (cond
      [(relative-path? p)
       (find-executable-path (find-system-path 'exec-file)
                             (find-system-path 'collects-dir))]
      [else
       p]))))
