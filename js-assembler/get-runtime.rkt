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
                ;; jquery is special: we need to make sure it's resilient against
                ;; multiple invokation and inclusion.
                jquery-protect-header.js
                jquery.js
                jquery-protect-footer.js
                
                jshashtable-2.1_src.js
                js-numbers.js
                
                baselib.js
                
                baselib_unionfind.js
                baselib_equality.js
                baselib_format.js
                
                baselib_lists.js
                baselib_vectors.js
                baselib_chars.js
                baselib_symbol.js
                baselib_strings.js
                baselib_bytes.js                    
                baselib_hash.js
                baselib_regexps.js
                baselib_paths.js
                baselib_boxes.js
                
                baselib_structs.js
                baselib_arity.js
                baselib_inspectors.js
                baselib_exceptions.js
                baselib_readergraph.js
                
                link.js
                types.js
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