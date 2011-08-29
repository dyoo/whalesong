#lang scribble/manual
@(require planet/scribble
          planet/version
          planet/resolver
          scribble/eval
          racket/sandbox
          racket/port
          (only-in racket/contract any/c)
          racket/runtime-path
          "scribble-helpers.rkt"
          "../js-assembler/get-js-vm-implemented-primitives.rkt")

@(require (for-label (this-package-in js))
          (for-label (this-package-in lang/base))
          (for-label (this-package-in resource))
          (for-label (this-package-in web-world)))

@(define-runtime-path whalesong-path "..")


@title{CS19 instructions for Whalesong}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]
