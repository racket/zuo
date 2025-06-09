#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "store.rkt"
         "free-vars.rkt"
         "skip.rkt"
         (only-in "set.rkt"
                  extend-end
                  subtract))

(provide -->gc/sfs)

(define -->gc/sfs
  (reduction-relation
   LC
   #:domain [tup
             g  ; current mode: (roots K) or sweep, where K is eval continuation
             K  ; accumulated K to return to eval
             Σ  ; from-space (doesn't change)
             A  ; accumulated map of allocations so far, from-space address to to-space address
             Σ  ; to-space (accumulated)
             S] ; accumulated from-space addresses still to be swept for newly reachable variables
   #:arrow -->gc/sfs
   (-->gc/sfs [tup (ctr roots (kcons (ctr t (tup e ρ_from)) K_root)) K Σ_from A Σ_to S]
              [tup (ctr roots K_root) (ksnoc K (ctr t (tup e ρ_to))) Σ_from A_′ Σ_to′ S_′]
              (where (set x ...) (free-vars e))
              (where (tup ρ_kept-to ρ_to A_′ Σ_to′ S_′) (retain-skip-env ρ_from (set x ...) Σ_from A Σ_to S))
              ;; ok to drop reference to `ρ_kept-to` when it's an extension of `ρ_to`
              root-env)
   (-->gc/sfs [tup (ctr roots (kcons (ctr t v) K_root)) K Σ_from A Σ_to S]
              [tup (ctr roots K_root) (ksnoc K (ctr t v_′)) Σ_from A_′ Σ_to S_′]
              (where (tup v_′ A_′ S_′) (retain-val v A S))
              ;; ok to drop reference to `ρ_kept` when it's an extension of `ρ_to`
              root-val)
   (-->gc/sfs [tup (ctr roots done) K Σ_from A Σ_to S]
              [tup sweep K Σ_from A Σ_to S]
              roots-done)
   (-->gc/sfs [tup sweep K Σ_from A Σ_to (scons (xtup σ_from (set x_live ...)) S)]
              [tup sweep K Σ_from A_′ Σ_to′′ S_′]
              (where σ_to (find A σ_from))
              (where (ctr env (tup x v ρ_from)) (fetch Σ_from σ_from))     ; original; `x` must be among the `x_live`s
              (where (tup v_′ A_v S_v) (retain-val v A S))
              (where (set x_next ...) (subtract (set x_live ...) (set x)))
              (where (tup ρ_kept-to ρ_to A_′ Σ_to′ S_′) (retain-skip-env ρ_from (set x_next ...) Σ_from A_v Σ_to S_v))
              (where Σ_to′′ (store Σ_to′ σ_to (ctr env (tup x v_′ ρ_kept-to))))
              sweep-env)
   (-->gc/sfs [tup sweep K Σ_from A Σ_to (scons (xtup σ_from (set)) S)]
              [tup sweep K Σ_from A_′ (extend-end Σ_to′ σ_to (ctr clos (tup e ρ_to))) S_′]
              (where σ_to (find A σ_from))
              (where (ctr clos (tup e ρ_from)) (fetch Σ_from σ_from))
              (where (set x ...) (free-vars e))
              (where (tup ρ_kept-to ρ_to A_′ Σ_to′ S_′) (retain-skip-env ρ_from (set x ...) Σ_from A Σ_to S))
              sweep-clos)))

(module+ pict
  (require "config.rkt"
           redex/pict
           "typeset.rkt"
           (only-in (submod "gc.rkt" pict) add-gc-signature))
  (provide gc-sfs-pict)
  (define gc-sfs-pict
    (using-rewriters
     (add-gc-signature
      (render-reduction-relation -->gc/sfs
                                 #:style 'horizontal)))))

(module+ test
  (require (submod ".." pict))
  gc-sfs-pict)
