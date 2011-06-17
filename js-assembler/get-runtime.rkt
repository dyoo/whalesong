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
;; followed by:
;;
;;     plt.link
;;     plt.helpers
;;     plt.types
;;     plt.primitives
;;     plt.runtime



(require racket/contract
         racket/runtime-path
         racket/port)



(provide/contract [get-runtime (-> string?)])
         
(define-runtime-path jquery.js "runtime-src/jquery.js")
(define-runtime-path hashtable.js "runtime-src/jshashtable-2.1_src.js")
(define-runtime-path jsnums.js "runtime-src/js-numbers.js")
(define-runtime-path link.js "runtime-src/link.js")

;; from js-vm
(define-runtime-path helpers.js "runtime-src/helpers.js")
;; from js-vm
(define-runtime-path types.js "runtime-src/types.js")
;; These primitives were coded for the js-vm project, and we'll gradually
;; absorb them in.
;(define-runtime-path js-vm-primitives.js "runtime-src/js-vm-primitives.js")

(define-runtime-path runtime.js "runtime-src/runtime.js")


;; The order matters here.  link needs to come near the top, because
;; the other modules below have some circular dependencies that are resolved
;; by link.
(define files (list jquery.js
                    hashtable.js
                    jsnums.js
                    link.js
                    helpers.js
                    types.js
;                    js-vm-primitives.js
                    runtime.js))



(define (path->string p)
  (call-with-input-file p
                (lambda (ip)
                  (port->string ip))))


(define text (apply string-append
                    (map path->string files)))


(define (get-runtime)
  text)