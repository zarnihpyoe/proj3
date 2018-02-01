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
(test (run '5) (v*s (numV 5) mt-store))
(test (run '(+ 2 3)) (v*s (numV 5) mt-store))
(test (run '(* 2 3)) (v*s (numV 6) mt-store))
(test (run '(- 5 3)) (v*s (numV 2) mt-store))
;; if0
(test (run '(if0 0 1 2)) (v*s (numV 1) mt-store))
(test (run '(if0 1 1 2)) (v*s (numV 2) mt-store))
(test (run '(if0 (- (- 2 3) -1) (* 2 2) (+ 1 5))) (v*s (numV 4) mt-store))
;; nested if0
(test (run '(if0 (if0 1 1 0)
                 (if0 0 4 5)
                 2)) (v*s (numV 4) mt-store))

;; functions
;; =========

;; function as a return value
(test (run '(fun (x) 0))
      (v*s (closV (list 'x) (numC 0) mt-env) mt-store))



;; exceptions
;; ==========

;; parse-time errors
;;; (test/exn (run '(+ 1 2 3)) "arguments")
;;; (test/exn (run '(fun (x x) 0)) "multiple")

;; interp-time errors



;; delete these...
;; =====================================
;;; (test/exn (desugar (parse '(seq) ))
;;;       "empty seq")
;;; (test (desugar (parse '(seq 1 2) ))
;;;       (seqC (numC 1) (numC 2)))
;;; (test (desugar (parse '(seq 0 1 2) ))
;;;       (seqC (numC 0)
;;;             (seqC (numC 1) (numC 2))))
