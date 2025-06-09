#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "store.rkt")

(provide -->interp)

(define -->interp
  (reduction-relation
   LC
   #:domain [tup m K Σ]
   #:arrow -->interp
   (-->interp [tup (tup (λ (x) e) ρ) K Σ]
              [tup (ctr obj σ) K Σ_′]
              (where (tup σ Σ_′) (close (λ (x) e) ρ Σ))
              lam)
   (-->interp [tup (tup (e_fun e_arg) ρ) K Σ]
              [tup (tup e_fun ρ) (kcons (ctr arg (tup e_arg ρ)) K) Σ]
              push)
   (-->interp [tup (tup (quote lit) ρ) K Σ]
              [tup (ctr prim lit) K Σ]
              lit)
   (-->interp [tup (tup x ρ) K Σ]
              [tup v K Σ]
              (where v (lookup x ρ Σ))
              var)
   (-->interp [tup v (kcons (ctr app (ctr obj σ)) K) Σ]
              [tup (tup e ρ_′) K Σ_′]
              (where (ctr clos (tup (λ (x) e) ρ)) (fetch Σ σ))
              (where (tup ρ_′ Σ_′) (bind x v ρ Σ))
              app)
   (-->interp [tup (ctr prim lit_arg) (kcons (ctr app (ctr prim lit_fun)) K) Σ]
              [tup (tup e_res mt) K Σ]
              (where e_res (primcall lit_fun lit_arg))
              prim)
   (-->interp [tup v_fun (kcons (ctr arg (tup e_arg ρ)) K) Σ]
              [tup (tup e_arg ρ) (kcons (ctr app v_fun) K) Σ]
              arg)))

(define-metafunction LC
  primcall : lit lit -> e
  [(primcall lit_fun lit_arg)
   e
   (side-condition (and (procedure? (term lit_fun))
                        (procedure-arity-includes? (term lit_fun) 1)))
   (where e ,((term lit_fun) (term lit_arg)))]
  [(primcall lit_fun lit_arg)
   (if (and (procedure? (term lit_fun))
            (procedure-arity-includes? (term lit_fun) 1))
       ,(error 'primcall "failure\n  fun: ~s\n  arg: ~s\n  res: ~s"
               (term lit_fun)
               (term lit_arg)
               ((term lit_fun) (term lit_arg)))
       ,(error 'primcall "failure\n  fun: ~s\n  arg: ~s" (term lit_fun) (term lit_arg)))])

(module+ pict
  (require "config.rkt"
           redex/pict
           "typeset.rkt"
           pict           
           (only-in (submod "grammar.rkt" pict) add-signature))
  (provide (all-defined-out))
  (define (add-interp-signature p)
    (add-signature (render-term
                    LC
                    (-->interp-jf [tup m K Σ] [tup m_′ K_′ Σ′]))
                   p))
  (define interp-pict
    (using-rewriters
     (add-interp-signature
      (render-reduction-relation -->interp
                                 #:style 'horizontal)))))

(module+ test
  (define e0
    (term ((λ (x) x) (quote 0))))
  (define e1
    (term (((λ (x) (λ (y) x)) (quote 1)) (quote 2))))
  (define e2
    (term ((quote ,(λ (x) (term (quote ,(+ x 1))))) (quote 2))))
  (test-->> -->interp
            (term [tup (tup ,e0 mt) done (set)])
            (term [tup (ctr prim 0) done (set
                                          (bnd 1 (ctr env (tup x (ctr prim 0) mt)))
                                          (bnd 0 (ctr clos (tup (λ (x) x) mt))))]))
  (test-->> -->interp
            (term [tup (tup ,e1 mt) done (set)])
            (term [tup (ctr prim 1) done (set
                                          (bnd 3 (ctr env (tup y (ctr prim 2) 1)))
                                          (bnd 2 (ctr clos (tup (λ (y) x) 1)))
                                          (bnd 1 (ctr env (tup x (ctr prim 1) mt)))
                                          (bnd 0 (ctr clos (tup (λ (x) (λ (y) x)) mt))))]))
  (test-->> -->interp
            (term [tup (tup ,e2 mt) done (set)])
            (term [tup (ctr prim 3) done (set)]))
  (require (submod ".." pict))
  interp-pict)
