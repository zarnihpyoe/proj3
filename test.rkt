#lang plai-typed
#| 
Testing the interpreter for
Top-level functions
|#

(require "mutation-starter.rkt")

;;(test (new-number) 1)
;;(test (new-number) 2)
;;(test (new-number-list 2) (list 3 4))
;;(test (new-number) 5)


;; basic ops tests
;; ===============

;; + * -
(test (run '5) (v*s (numV 5) empty))
(test (run '(+ 2 3)) (v*s (numV 5) empty))
(test (run '(* 2 3)) (v*s (numV 6) empty))
(test (run '(- 5 3)) (v*s (numV 2) empty))
;; if0
(test (run '(if0 0 1 2)) (v*s (numV 1) empty))
(test (run '(if0 1 1 2)) (v*s (numV 2) empty))
(test (run '(if0 (- (- 2 3) -1) (* 2 2) (+ 1 5))) (v*s (numV 4) empty))
;; nested if0
(test (run '(if0 (if0 1 1 0)
                 (if0 0 4 5)
                 2)) (v*s (numV 4) empty))

;; exceptions
;; ==========

;; parse-time errors
;;; (test/exn (run '(+ 1 2 3)) "arguments")
;;; (test/exn (run '(fun (x x) 0)) "multiple")

;; interp-time errors

