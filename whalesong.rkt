#lang racket/base

(require racket/cmdline)

;; Usage:
;;
;;
;; * Get complete list of dependency modules up to kernel
;;
;;     $ whalesong deps 
;;
;;
;; * Compile JavaScript object files (.jso)
;;
;;     $ whalesong compile [file.rkt] ...
;;
;;
;; * Build standalone application
;;
;;     $ whalesong build main-module-name.rkt


