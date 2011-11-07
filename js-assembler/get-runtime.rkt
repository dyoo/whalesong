#lang racket/base

;; Function to get the runtime library.
;;
;; The resulting Javascript will produce a file that loads:
;;
;;
;; jquery at the the toplevel
;; HashTable at the toplevel
;; jsnums at the toplevel
;;
;; followed by the base library
;;



(require racket/contract
         racket/runtime-path
         racket/port)



(provide/contract [get-runtime (-> string?)])


(define-runtime-path base-path "runtime-src")


;; The order matters here.  link needs to come near the top, because
;; the other modules below have some circular dependencies that are resolved
;; by link.
(define files '(
                top.js
                
                ;; jquery is special: we need to make sure it's resilient against
                ;; multiple invokation and inclusion.
                jquery-protect-header.js
                jquery.js
                jquery-protect-footer.js
                
                js-numbers.js
                
                baselib.js

                baselib-frames.js
                
                baselib-unionfind.js
                baselib-equality.js
                baselib-format.js

                baselib-constants.js
                baselib-numbers.js
                baselib-lists.js
                baselib-vectors.js
                baselib-chars.js
                baselib-symbols.js
                baselib-strings.js
                baselib-bytes.js                    

                hashes-header.js
                jshashtable-2.1_src.js
                llrbtree.js
                baselib-hashes.js
                hashes-footer.js

                
                baselib-regexps.js
                baselib-paths.js
                baselib-boxes.js
                baselib-placeholders.js
                baselib-keywords.js
                baselib-structs.js
                baselib-srclocs.js
                baselib-ports.js
                baselib-functions.js
                baselib-modules.js
                baselib-contmarks.js
                
                baselib-arity.js
                baselib-inspectors.js
                baselib-exceptions.js
                baselib-readergraph.js

                ;; baselib-check has to come after the definitions of types,
                ;; since it uses the type predicates immediately on init time.
                baselib-check.js

                baselib-primitives.js
                runtime.js))



(define (path->string p)
  (call-with-input-file p
                (lambda (ip)
                  (port->string ip))))


(define text (apply string-append
                    (map (lambda (n)
                           (path->string 
                            (build-path base-path (symbol->string n))))
                         files)))

(define (get-runtime)
  text)