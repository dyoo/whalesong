#lang racket/base
(require racket/list
	 "../main.rkt"
	 racket/runtime-path
	 racket/system
	 "check-coverage.rkt")

(define-runtime-path my-directory ".")

(when (find-executable-path "node")
  (printf "Running the VM internal test suite\n")
  (parameterize ([current-directory my-directory])
    (system "unit-tests/run-tests")
    (printf "Press Enter to continue.\n")
    (void (read-line))))


(printf "Checking for untouched wescheme primitives\n")
(unless (empty? (untouched-wescheme-primitives))
  (print-coverage-report)
  (printf "Press Enter to continue.\n")
  (void (read-line)))

(printf "Running browser tests\n")
(run-in-browser "all-tests.rkt")
