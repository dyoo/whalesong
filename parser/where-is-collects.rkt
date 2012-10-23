#lang typed/racket/base
(require/typed racket/path
               (normalize-path (Path -> Path)))

(provide collects-path)

(define collects-path
  (normalize-path
   (let: ([p : Path (find-system-path 'collects-dir)])
     (cond
      [(relative-path? p)
       (define maybe-path (find-executable-path (find-system-path 'exec-file)
                                                (find-system-path 'collects-dir)))
       (cond
         [(path? maybe-path)
          maybe-path]
         [else
          (error 'collects-path "Could not find collects path")])]
      [else
       p]))))
