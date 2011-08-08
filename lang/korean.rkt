#lang s-exp "base.rkt"

(require (for-syntax racket/base))


(define-syntax 정의
  (make-rename-transformer #'define))


(define-syntax 정의-구조
  (make-rename-transformer #'define-struct))


(define-syntax 지역
  (make-rename-transformer #'local))


(define-syntax 조건부
  (make-rename-transformer #'cond))


(define-syntax 다른
  (make-rename-transformer #'else))


(define-syntax 면
  (make-rename-transformer #'if))


(define-syntax 케이스
  (make-rename-transformer #'case))


(define-syntax 람다
  (make-rename-transformer #'lambda))


(define-syntax 수정
  (make-rename-transformer #'set!))


(define-syntax 또는
  (make-rename-transformer #'or))


(define-syntax 과
  (make-rename-transformer #'and))


(define-syntax 필요
  (make-rename-transformer #'require))