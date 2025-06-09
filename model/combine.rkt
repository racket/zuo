#lang racket
(require redex/reduction-semantics
         "grammar.rkt")

(provide combine)

;; Combine an `interp` reduction with a `gc` reduction by switching
;; between `eval` and `gc` modes. In `eval` mode, there's fuel.
;; As long as fuel is nonnegative, then evaluation can
;; proceed. When fuel becomes negative, then we GC, and the resulting
;; store size is used as the new fuel. Since GC should take time
;; proportional to the result store, using the store size as eval
;; fuel mean that we give eval and GC equal time, so we can consider
;; it as a constant-factor on evaluation time (i.e., doesn't change
;; asymptotic complexity). A real implementation would only decrement
;; fuel when allocation occurs, effectively.
(define (combine -->interp -->gc)
  (define-judgment-form LC
    #:mode (-->interp-jf I O)
    [(where ([tup m_new K_new Σ_new])
            ,(apply-reduction-relation
              -->interp
              (term [tup m K Σ])))
     ------
     (-->interp-jf [tup m K Σ] [tup m_new K_new Σ_new])])
  (define-judgment-form LC
    #:mode (-->gc-jf I O)
    [(where ([tup g_new K_new Σ_new-from A_new Σ_new-to S_new])
            ,(apply-reduction-relation
              -->gc
              (term [tup g K Σ_from A Σ_to S])))
     ------
     (-->gc-jf [tup g K Σ_from A Σ_to S]
               [tup g_new K_new Σ_new-from A_new Σ_new-to S_new])])
  (define -->interp+gc
    (reduction-relation
     LC
     #:domain h
     (--> [tup eval n m K Σ]
          [tup eval (sub1 n) m_′ K_′ Σ_′]
          (judgment-holds (≥ n 0))
          (judgment-holds (-->interp-jf [tup m K Σ] [tup m_′ K_′ Σ_′]))
          eval)
     (--> [tup eval -1 m K Σ]
          [tup gc 0 (ctr roots (kcons (ctr ret m) K)) done Σ (set) (set) (seq)]
          start-gc)
     (--> [tup gc n g K Σ_from A Σ_to S]
          [tup gc (add1 n) g_′ K_′ Σ_from′ A_′ Σ_to′ S_′]
          (judgment-holds (-->gc-jf [tup g K Σ_from A Σ_to S] [tup g_′ K_′ Σ_from′ A_′ Σ_to′ S_′]))
          gc)
     (--> [tup gc n_gc sweep (kcons (ctr ret m) K) Σ_from A Σ_to (seq)] ; empty S
          [tup eval n_gc m K Σ_to]
          end-gc)))
  -->interp+gc)

(define-judgment-form LC
  #:mode (≥ I I)
  [(side-condition ,(>= (term any_1) (term any_2)))
   -----
   (≥ any_1 any_2)])

(define-metafunction LC
  [(sub1 n) ,(- (term n) 1)])

(define-metafunction LC
  [(add1 n) ,(+ (term n) 1)])

(define-metafunction LC
  [(len-Σ Σ) ,(length (term Σ))])

(module+ pict
  (require "config.rkt"
           redex/pict
           pict
           "interp.rkt"
           "gc.rkt"
           "typeset.rkt")
  (provide combine-pict)
  (define combine-pict
    (using-rewriters
      (vr-append
       10
       (frame (inset (render-term
                      LC
                      (-->combine-jf h h_′))
                     10 4))
       (render-reduction-relation (combine -->interp -->gc)
                                  #:style 'horizontal)))))

(module+ test
  (require (submod ".." pict) "typeset.rkt" redex/pict)
  combine-pict)
