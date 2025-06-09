#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "store.rkt"
         (only-in "set.rkt"
                  extend-end))

(provide -->gc)

;; Model garbage collection as a small-step reduction
;; so that we can treat the number of needed steps as
;; its running time.
(define -->gc
  (reduction-relation
   LC
   #:domain [tup
             g  ; current mode: (roots K) or sweep, where K is eval continuation
             K  ; accumulated K to return to eval
             Σ  ; from-space (doesn't change)
             A  ; accumulated map of allocations so far, from-space address to to-space address
             Σ  ; to-space (accumulated)
             S] ; accumulated from-space addresses still to be copied
   #:arrow -->gc
   (-->gc [tup (ctr roots (kcons (ctr t (tup e ρ_from)) K_root)) K Σ_from A Σ_to S]
          [tup (ctr roots K_root) (ksnoc K (ctr t (tup e ρ_to))) Σ_from A_′ Σ_to S_′]
          (where (tup ρ_to A_′ S_′) (retain-env ρ_from A S)) ; allocate in to-space, leaving copy and sweep to later
          root-env)
   (-->gc [tup (ctr roots (kcons (ctr t v) K_root)) K Σ_from A Σ_to S]
          [tup (ctr roots K_root) (ksnoc K (ctr t v_′)) Σ_from A_′ Σ_to S_′]
          (where (tup v_′ A_′ S_′) (retain-val v A S))
          root-val)
   (-->gc [tup (ctr roots (kcons (ctr t (ctr flat (tup e (set x ...) ρ ρ_flat))) K)) K_done Σ_from A Σ_to S]
          [tup (ctr roots K) (ksnoc K_done (ctr t (ctr flat (tup e (set x ...) ρ_′ ρ_flat′)))) Σ_from A_′′ Σ_to S_′′]
          (where (tup ρ_′ A_′ S_′) (retain-env ρ A S))
          (where (tup ρ_flat′ A_′′ S_′′) (retain-env ρ_flat A_′ S_′))
          root-flat)
   (-->gc [tup (ctr roots done) K Σ_from A Σ_to S]
          [tup sweep K Σ_from A Σ_to S]
          roots-done)
   (-->gc [tup sweep K Σ_from A Σ_to (scons (xtup σ_from (set)) S)]
          [tup sweep K Σ_from A_′′ (extend-end Σ_to σ_to (ctr env (tup x v_′ ρ_to))) S_′′]
          (where σ_to (find A σ_from))                   ; find allocated address in to-space
          (where (ctr env (tup x v ρ_from)) (fetch Σ_from σ_from))  ; find original to copy in from-space
          (where (tup v_′ A_′ S_′) (retain-val v A S))
          (where (tup ρ_to A_′′ S_′′) (retain-env ρ_from A_′ S_′))
          sweep-env)
   (-->gc [tup sweep K Σ_from A Σ_to (scons (xtup σ_from (set)) S)]
          [tup sweep K Σ_from A_′ (extend-end Σ_to σ_to (ctr clos (tup e ρ_to))) S_′]
          (where σ_to (find A σ_from))
          (where (ctr clos (tup e ρ_from)) (fetch Σ_from σ_from))
          (where (tup ρ_to A_′ S_′) (retain-env ρ_from A S))
          sweep-clos)))

(module+ pict
  (require "config.rkt"
           redex/pict
           pict
           "typeset.rkt"
           (only-in (submod "grammar.rkt" pict) add-signature))
  (provide (all-defined-out))
  (define (add-gc-signature p)
    (add-signature
     (render-term
      LC
      (-->gc-jf [tup g K Σ_from A Σ_to S] [tup g_′ K_′ Σ_from A_′ Σ_to′ S_′]))
     p))
  (define-values (gc-pict gc-flat-rule-pict)
    (using-rewriters
     #:xtup? #f
     (values
      (add-gc-signature
       (parameterize ([render-reduction-relation-rules '(root-env root-val roots-done sweep-env sweep-clos)])
         (render-reduction-relation -->gc
                                    #:style 'horizontal)))
      (parameterize ([render-reduction-relation-rules '(root-flat)])
        (render-reduction-relation -->gc
                                   #:style 'horizontal))))))

(module+ test
  (define store1 (term (set (bnd 0 (ctr env (tup x (ctr prim 0) mt))))))
  (test-->> -->gc
            (term [tup (ctr roots (kcons (ctr ret (tup (quote 0) mt)) done)) done,store1 (set) (set) (seq)])
            (term [tup sweep (kcons (ctr ret (tup (quote 0) mt)) done) ,store1 (set) (set) (seq)]))
  (define store2 (term (set (bnd 0 (ctr env (tup x (ctr prim 0) mt))) (bnd 1 (ctr env (tup x (ctr prim 9) mt))))))
  (test-->> -->gc
            (term [tup (ctr roots (kcons (ctr ret (tup (quote 0) 0)) done)) done ,store2 (set) (set) (seq)])
            (term [tup sweep (kcons (ctr ret (tup (quote 0) 0)) done) ,store2 (set (bnd 0 (xtup 0 (set)))) (set (bnd 0 (ctr env (tup x (ctr prim 0) mt)))) (seq)]))
  (test-->> -->gc
            (term [tup (ctr roots (kcons (ctr ret (tup (quote 0) 1)) done)) done ,store2 (set) (set) (seq)])
            (term [tup sweep (kcons (ctr ret (tup (quote 0) 0)) done) ,store2 (set (bnd 1 (xtup 0 (set)))) (set (bnd 0 (ctr env (tup x (ctr prim 9) mt)))) (seq)]))
  (define store3 (term (set (bnd 0 (ctr env (tup y (ctr prim 0) mt))) (bnd 1 (ctr env (tup x (ctr obj 2) mt))) (bnd 2 (ctr clos (tup (λ (x) x) 0))))))
  (test-->> -->gc
            (term [tup (ctr roots (kcons (ctr ret (tup (quote 0) 1)) done)) done ,store3 (set) (set) (seq)])
            (term [tup sweep (kcons (ctr ret (tup (quote 0) 0)) done) ,store3 (set (bnd 0 (xtup 2 (set)))
                                                                                   (bnd 2 (xtup 1 (set)))
                                                                                   (bnd 1 (xtup 0 (set))))
                       (set (bnd 0 (ctr env (tup x (ctr obj 1) mt))) (bnd 1 (ctr clos (tup (λ (x) x) 2))) (bnd 2 (ctr env (tup y (ctr prim 0) mt))))
                       (seq)]))
  (define store4 (term (set (bnd 0 (ctr env (tup y (ctr prim 0) mt))) (bnd 1 (ctr env (tup x (ctr obj 2) 0))) (bnd 2 (ctr clos (tup (λ (x) x) mt))))))
  (test-->> -->gc
            (term [tup (ctr roots (kcons (ctr ret (tup (quote 0) 1)) done)) done ,store4 (set) (set) (seq)])
            (term [tup sweep (kcons (ctr ret (tup (quote 0) 0)) done) ,store4 (set (bnd 0 (xtup 2 (set)))
                                                                                   (bnd 2 (xtup 1 (set)))
                                                                                   (bnd 1 (xtup 0 (set))))
                       (set (bnd 0 (ctr env (tup x (ctr obj 1) 2))) (bnd 1 (ctr clos (tup (λ (x) x) mt))) (bnd 2 (ctr env (tup y (ctr prim 0) mt))))
                       (seq)]))

  (require (submod ".." pict))
  gc-pict
  gc-flat-rule-pict)
