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

;; function literals
(test (run '(fun (x) 0))
      (v*s (closV (list 'x) (numC 0) mt-env) mt-store))
;; applying function literals
(test (run '((fun (x) 0) 3) )
      (v*s (numV 0) (list (cell 1 (numV 3)))))
(test (run '((fun (x) (+ 2 x)) 4) )
      (v*s (numV 6) (list (cell 2 (numV 4)))))


;; with
;; ====

;; simeple with
(test (run '(with ([x 5]) x))
     (v*s (numV 5) (list (cell 3 (numV 5)))))
;; with + named function definition
(test (run '(with ([f (fun (x) (* x 2))]) (f 5)))
      (v*s (numV 10) (list [cell 5 (numV 5)]
                           [cell 4 (closV (list 'x) (multC (idC 'x) (numC 2)) mt-env)])))
;; double binding, same variable name 'x' for argument of f and variable 'x', result in no conflict
(test (run '(with ([f (fun (x) (* x 2))]
                   [x 5]) (f x)))
      (v*s (numV 10) (list [cell 8 (numV 5)]
                           [cell 7 (numV 5)]
                           [cell 6 (closV (list 'x) (multC (idC 'x) (numC 2)) mt-env)])))


;; boxes
;; =====

;; box literals
(test (run '(box (+ 4 6)))
      (v*s (boxV 9) (list (cell 9 (numV 10)))))
;; unbox box literals
(test (run '(unbox (box 11)))
     (v*s (numV 11) (list (cell 10 (numV 11)))))
;; setbox box literals
(test (run '(setbox (box 12) 13))
      (v*s (numV 13) (list (cell 11 (numV 13)))))

;; multiple boxes + with, change value of x, both boxV of x and y don't change
(test (run-v '(with ([x (box 10)])
                    (with ([y x])
                          (seq (setbox x 11) y)))) (boxV 12))
;; but the value of both x and y change
(test (run-v '(with ([x (box 10)])
                    (with ([y x])
                          (seq (setbox x 11) (unbox y))))) (numV 11))
;; this time, use set instend of setbox, the value of x is changed, but not that of y
(test (run-v '(with ([x (box 10)])
                    (with ([y x])
                          (seq (set x (box 11))
                               (unbox y))))) (numV 10))
;; the value of y got overrided
(test (run-v '(with ([x (box 10)]
                     [y (box 12)])
                    (with ([y x])
                          (seq (setbox x 11)
                               (unbox y))))) (numV 11))




;; exceptions
;; ==========

;; parse-time errors
;;; (test/exn (run '(+ 1 2 3)) "arguments")
;;; (test/exn (run '(fun (x x) 0)) "multiple")

;; interp-time errors
(test/exn (run '(set a 0)) "unbound identifier")


;; delete these...
;; =====================================
;;; (test/exn (desugar (parse '(seq) ))
;;;       "empty seq")
;;; (test (desugar (parse '(seq 1 2) ))
;;;       (seqC (numC 1) (numC 2)))
;;; (test (desugar (parse '(seq 0 1 2) ))
;;;       (seqC (numC 0)
;;;             (seqC (numC 1) (numC 2))))

;;; (test (interp-list (list (numC 1) (numC 2) (numC 3))
;;;                     mt-env mt-store)
;;;       (vs*s (list [numV 1] [numV 2] [numV 3]) mt-store))
