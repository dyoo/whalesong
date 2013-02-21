#lang racket
(require (planet dyoo/whalesong/get-module-bytecode)
         (planet dyoo/whalesong/parser/parse-bytecode)
         (planet dyoo/whalesong/compiler/compiler)
         (planet dyoo/whalesong/compiler/compiler-structs)
         (planet dyoo/whalesong/js-assembler/assemble))

(define bytecode 
  (get-module-bytecode 
   (open-input-string 
    (string-append "#lang planet dyoo/whalesong\n"
                   "(define (f x)\n"
                   "  (if (= x 0)\n"
                   "      1\n"
                   "      (* x (f (sub1 x)))))\n\n"
                   "(provide f)"))))

(define ast (parse-bytecode (open-input-bytes bytecode)))

(define stmts (compile ast 'val next-linkage/drop-multiple))

(define op (open-output-string))
(assemble/write-invoke stmts op)
(define js-code (get-output-string op))