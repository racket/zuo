#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "store.rkt"
         "free-vars.rkt"
         "interp.rkt"
         "skip.rkt")

(provide -->interp/sfs)

;; This SFS variant of `interp` replaces the rule to form a closure
;; and to start evaluating subexpressions. Pruning the enviornment
;; by forming flat closures is performed one step at a time so that
;; the number of reductions corresponds to realistic running time.
(define -->interp/sfs
  (extend-reduction-relation
   -->interp
   LC
   #:domain [tup m K Σ]
   #:arrow -->interp/sfs
   (-->interp/sfs [tup (tup (λ (x) e) ρ) K Σ]
                  [tup (ctr flat (tup (λ (x) e) (set x_free ...) ρ_live mt)) K Σ]
                  (where (set x_free ...) (free-vars (λ (x) e)))
                  (where ρ_live (skip-to ρ (set x_free ...) Σ))
                  lam)
   (-->interp/sfs [tup (tup (e_fun e_arg) ρ) K Σ]
                  [tup (ctr flat (tup (e_fun e_arg) (set x_free ...) ρ_live mt)) (kcons (ctr ret (tup e_fun ρ)) K) Σ]
                  (where (set x_free ...) (free-vars e_arg))
                  (where ρ_live (skip-to ρ (set x_free ...) Σ))
                  push)

   (-->interp/sfs [tup (ctr flat (tup e (set x x_rest ...) ρ ρ_flat)) K Σ]
                  [tup (ctr flat (tup e (set x_rest ...) ρ_next ρ_′)) K Σ_′]
                  (where v (lookup x ρ Σ))
                  (where (tup ρ_′ Σ_′) (bind x v ρ_flat Σ))
                  (where ρ_next (skip-to ρ (set x_rest ...) Σ))
                  flat)

   (-->interp/sfs [tup (ctr flat (tup (λ (x) e) (set) mt ρ_flat)) K Σ]
                  [tup (ctr obj σ) K Σ_′]
                  (where (tup σ Σ_′) (close (λ (x) e) ρ_flat Σ))
                  flat-lam)
   (-->interp/sfs [tup (ctr flat (tup (e_fun e_arg) (set) mt ρ_flat)) (kcons (ctr ret (tup e_fun ρ)) K) Σ]
                  [tup (tup e_fun ρ) (kcons (ctr arg (tup e_arg ρ_flat)) K) Σ]
                  flat-push)))

(module+ pict
  (require "config.rkt"
           redex/pict
           "typeset.rkt"
           (only-in (submod "interp.rkt" pict) add-interp-signature))
  (provide interp-sfs-pict)
  (define interp-sfs-pict
    (using-rewriters
     (add-interp-signature
      (render-reduction-relation -->interp/sfs
                                 #:style 'horizontal)))))

(module+ test
  (require (submod ".." pict))
  interp-sfs-pict)
