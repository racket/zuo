#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "interp.rkt"
         "test-run.rkt")

;; One way to check a space bound is to make sure something loops forever
;; in bounded space, which means a cycle in the reduction graph.

;; Long enough to check for loops right now, but could turn up for more testing:
(define MANY-STEPS 256)

;; Function to check that a cycle is found in the reduction graph (when `stop-at`
;; is #f), or that it keeps going at at least `stop-at` steps.
(define (test-loops -> t
                    #:stop-at [stop-at #f])
  (let loop ([seen (hash t #t)] [t t] [n 0])
    (define ts (apply-reduction-relation -> t))
    (when (null? ts)
      (println t)
      (error "no more steps"))
    (unless (= 1 (length ts))
      (println t)
      (for ([t (in-list ts)])
        (println t))
      (error "not a deterministic step"))
    (define new-t (car ts))
    (cond
      [(hash-ref seen new-t #f)
       (when stop-at (error "looped before stopping point"))]
      [(eqv? n stop-at)
       (void)]
      [else
       (loop (hash-set seen (car ts) #t) new-t (add1 n))])))

;; Check that Omega doesn't grow unboundedly
(define Ω (term ((λ (x) (x x)) (λ (x) (x x)))))

(test-loops -->interp+gc
            (term [tup eval 0 (tup ,Ω mt) done (set)]))
(test-loops -->interp
            #:stop-at MANY-STEPS
            (term [tup (tup ,Ω mt) done (set)]))


;; CEK-leak loop in introduction
(define CEK-leak
  (term ((λ (f) ((f f) (λ (x) x)))
         (λ (f) (λ (y) ((f f) (λ (z) z)))))))

(test-loops -->interp+gc
            #:stop-at MANY-STEPS
            (term [tup eval 0 (tup ,CEK-leak mt) done (set)]))
(test-loops -->interp/sfs+gc
            (term [tup eval 0 (tup ,CEK-leak mt) done (set)]))
(test-loops -->interp+gc/sfs
            (term [tup eval 0 (tup ,CEK-leak mt) done (set)]))
                         
(define (mk body)
  (term [tup eval 0 (tup
                     ((λ (f)
                        (((f f) (quote 0)) (quote 0)))
                      (λ (f)
                        (λ (y)
                          (λ (x)
                            (((f f)
                              (λ (z)
                                ,body))
                             (quote 0))))))
                     mt)
             done
             (set)]))
(test-loops -->interp/sfs+gc
            (mk (term (quote 0))))
(test-loops -->interp+gc
            #:stop-at MANY-STEPS
            (mk (term (quote 0))))
(test-loops -->interp/sfs+gc
            #:stop-at MANY-STEPS
            (mk (term y)))

(test-loops -->interp+gc/sfs
            (mk (term (quote 0))))
(test-loops -->interp+gc/sfs
            #:stop-at MANY-STEPS
            (mk (term y)))
