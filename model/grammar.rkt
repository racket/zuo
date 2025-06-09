#lang racket
(require redex/reduction-semantics)

(provide LC
         λet)

(define-language LC
  ;; Expressions
  (e ::= (λ (x) e) x (e e) (quote lit))

  ;; Values
  (v ::= (ctr obj σ) (ctr prim lit))

  ;; Primitive values
  (lit ::= any)

  ;; Expression with environment, value, or partial closure
  (m ::= (tup e ρ) v
         (ctr flat (tup e (set x ...) ρ ρ)))

  ;; Just for typesetting:
  (m/short ::= (tup e ρ) v)

  (x ::= variable-not-otherwise-mentioned)

  ;; Continuations
  (K ::= done (kcons k K))  ; no need to allocate, since no `call/cc`
  (k ::= (ctr t m))
  (t ::= arg app
         ret) ; for GC return

  ;; Addresses and store for environments
  (σ ::= integer)
  (ρ ::= σ mt)    ; environment is just an address or empty
  (Σ ::= (set (bnd σ b) ...)) ; store maps addresses to environment extensions

  ;; Allocations
  (b ::= (ctr env (tup x v ρ))    ; environment extension, chains to rest of the environment
         (ctr clos (tup e ρ)))    ; closure

  (A ::= (set (bnd σ (xtup σ L)) ...)) ; allocations/forwards during a GC: from-space address to to-space addresss + live sets
  (S ::= (seq) (scons (xtup σ (set x ...)) S)) ; from-space addresses that need copying and sweeping for reachable variables

  (L ::= (set (set x ...) ...)) ; live-variable sets; records in `A` which sets have been traced for GC SFS

  ;; Evaluator versus GC state for combined reduction
  (h ::= [tup eval n m K Σ]      ; eval as long as n >= 0
         [tup gc n g K Σ A Σ S]) ; collect and count back up via n

  ;; GC reduction state
  (g ::= (ctr roots K) sweep)  ; GC stage: sweeping reached objects

  ;; Misc
  (n ::= integer))

(define-extended-language LC+flat LC
  (m ::= .... (ctr flat (tup e (set x ...) ρ ρ))))

(define-metafunction LC
  λet : variable e e -> e
  [(λet x e_rhs e_body)
   ((λ (x) e_body) e_rhs)])

(define-language Empty)

(module+ pict
  (require "config.rkt"
           pict
           redex/pict
           "typeset.rkt")
  (provide (all-defined-out))
  (define grammar-pict
    (using-rewriters
     (vl-append
      5
      (ht-append
       20
       (render-language LC #:nts '(e v m/short x))
       (render-language LC #:nts '(t k K))
       (render-language LC #:nts '(σ Σ ρ b)))
     (render-metafunction λet))))
  (define sfs-grammar-pict
    (using-rewriters
     (render-language LC+flat #:nts '(m))))
  (define gc-grammar-pict
    (using-rewriters
     #:xtup? #f
     (ht-append
      20
      (render-language LC #:nts '(A S))
      (render-language LC #:nts '(g n)))))
  (define combine-grammar-pict
    (using-rewriters
     (render-language LC #:nts '(h))))
  (define gc-sfs-grammar-pict
    (using-rewriters
     (ht-append
      20
      (render-language LC #:nts '(S))
      (render-language LC #:nts '(A L)))))
  (define (add-signature sig p)
    (vr-append
     10
     (frame (inset sig 10 4))
     p))
  (define-values (e-pict
                  v-pict
                  m-pict
                  x-pict
                  K-pict
                  k-pict
                  t-pict
                  Sigma-pict
                  sigma-pict
                  rho-pict
                  b-pict
                  A-pict
                  S-pict
                  L-pict
                  g-pict
                  arg-k-pict
                  app-k-pict
                  ret-k-pict
                  flat-m-pict
                  true-pict
                  quote-lit-pict
                  obj-sigma-pict
                  prim-lit-pict
                  e-rho-pict
                  flat-e-rho-pict
                  mt-pict
                  env-x-v-rho-pict
                  clos-e-rho-pict
                  lookup-mf-pict
                  Sigma-from-pict
                  Sigma-to-pict
                  sigma-from-pict
                  sigma-to-pict
                  sigma-to-L-pict
                  roots-pict
                  sweep-pict
                  retain-env-mf-pict
                  retain-val-mf-pict
                  retain-mf-pict
                  free-vars-mf-pict
                  skip-to-mf-pict
                  kept-skip-to-mf-pict
                  retain-skip-env-mf-pict
                  retain-for-mf-pict
                  refine-mf-pict
                  quote-zero-pict
                  rho-flat-pict
                  rho-next-pict
                  rho-kept-pict
                  rho-prev-pict
                  rho-live-pict
                  cek-leak-pict
                  y-var-pict
                  identity-z-pict
                  sfs-quad-pict
                  sfs-quad-result-pict
                  x-1-var-pict
                  x-N-var-pict
                  x-i-var-pict
                  sfs-quad-full-pict
                  R-e-pict
                  D-e-pict
                  library-def-pict
                  w-var-pict
                  x-var-pict
                  z-var-pict)
    (using-rewriters
     (values
      (render-term LC e)
      (render-term LC v)
      (render-term LC m)
      (render-term LC x)
      (render-term LC K)
      (render-term LC k)
      (render-term LC t)
      (render-term LC Σ)
      (render-term LC σ)
      (render-term LC ρ)
      (render-term LC b)
      (render-term LC A)
      (render-term LC S)
      (render-term LC L)
      (render-term LC g)
      (render-term LC arg)
      (render-term LC app)
      (render-term LC ret)
      (render-term LC flat)
      (render-term LC (λ (x) (λ (y) x)))
      (render-term LC 'lit)
      (render-term LC (ctr obj σ))
      (render-term LC (ctr prim lit))
      (render-term LC (tup e ρ))
      (render-term LC (flat (tup e (set x ...) ρ ρ)))
      (render-term LC mt)
      (render-term LC (ctr env (tup x v ρ)))
      (render-term LC (ctr clos (tup e ρ)))
      (render-term LC lookup)
      (render-term LC Σ_from)
      (render-term LC Σ_to)
      (render-term LC σ_from)
      (render-term LC σ_to)
      (render-term LC (tup σ_to L))
      (render-term LC roots)
      (render-term LC sweep)
      (render-term LC retain-env)
      (render-term LC retain-val)
      (render-term LC retain)
      (render-term LC free-vars)
      (render-term LC skip-to)
      (render-term LC kept-skip-to)
      (render-term LC retain-skip-env)
      (render-term LC retain-for)
      (render-term LC refine)
      (render-term LC '0)
      (render-term LC ρ_flat)
      (render-term LC ρ_next)
      (render-term LC ρ_kept)
      (render-term LC ρ_prev)
      (render-term LC ρ_live)
      (render-term LC (λet f (λ (f) (λ (y)
                                      ((f f) (λ (z) z))))
                           ((f f) (λ (x) x))))
      (render-term LC y)
      (render-term LC (λ (z) z))
      (render-term LC (λ (x_1) ... (λ (x_N) (λ (y) (λ (z) (x_1 ... x_N))))))
      (render-term LC (λ (z) (x_1 ... x_N)))
      (render-term LC x_1)
      (render-term LC x_N)
      (render-term LC x_i)
      (render-term LC
                   (λet D (λ (x_1) ... (λ (x_N) (λ (y) (λ (z) (x_1 ... x_N)))))
                        (λet R ((D '1) ... '_N)
                             (((_foldn '_N) (λ (a) ((_cons a) (R '0)))) _empty))))
      (render-term LC R)
      (render-term LC D)
      (render-term LC
                   (vert
                    (horiz _true = (λ (x) (λ (y) x)))
                    (horiz _false = (λ (x) (λ (y) y)))
                    (horiz _sel = (λ (q) (λ (x) (λ (y) (((q x) y) '0)))))
                    (horiz _pair = (λ (a) (λ (d) (λ (s) ((s a) d)))))
                    (horiz _empty = ((_pair _false) _false))
                    (horiz _cons = (λ (a) (λ (d) ((_pair _true) ((_pair a) d)))))
                    (horiz _foldn = (λ (N)
                                      (λ (R)
                                        (λ (a)
                                          ((λ (f)
                                             (((f f) a) N))
                                           (λ (f)
                                             (λ (a)
                                               (λ (n)
                                                 (((sel ((quote zero?) n))
                                                   (λ (d) a))
                                                  (λ (d)
                                                    (((f f) (R a)) (((quote minus) n) (quote 1)))))))))))))))
      (render-term Empty w)
      (render-term Empty x)
      (render-term Empty z))))
  (define (render-label str)
    (using-rewriters
     (text (string-append "[" str "]") (label-style) (default-font-size)))))

(module+ test
  (require (submod ".." pict))
  grammar-pict
  gc-grammar-pict
  combine-grammar-pict
  sfs-grammar-pict
  gc-sfs-grammar-pict
  quote-lit-pict
  obj-sigma-pict
  prim-lit-pict
  sfs-quad-pict
  sfs-quad-full-pict
  library-def-pict
  cek-leak-pict)
