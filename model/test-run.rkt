#lang racket
(require redex/reduction-semantics
         rackunit
         "grammar.rkt"
         "interp.rkt"
         "interp-sfs.rkt"
         "gc.rkt"
         "gc-sfs.rkt"
         "combine.rkt")

(provide -->interp+gc
         -->interp/sfs+gc
         -->interp+gc/sfs

         plus
         minus
         is-zero?

         make-fib)

(define -->interp+gc (combine -->interp -->gc))
(define -->interp/sfs+gc (combine -->interp/sfs -->gc))
(define -->interp+gc/sfs (combine -->interp -->gc/sfs))

(define (run -->interp+gc e-term)
  (define-metafunction LC
    run : e -> v
    [(run e)
     v
     (where ([tup eval n v done Σ])
            ,(apply-reduction-relation*
              -->interp+gc
              (term [tup eval 0 (tup e mt) done (set)])))])
  (term (run ,e-term)))

(define (run-interp -->interp e-term)
  (define-metafunction LC
    run : e -> v
    [(run e)
     v
     (where ([tup v done Σ])
            ,(apply-reduction-relation*
              -->interp
              (term [tup (tup e mt) done (set)])))])
  (term (run ,e-term)))

(module library racket/base
  (provide (except-out (all-defined-out)
                       term
                       zero?))

  (define-syntax-rule (term e) (quasiquote e))
  
  (define plus
    (lambda (x)
      `(quote ,(λ (y)
                 `(quote ,(+ x y))))))
  (define minus
    (lambda (x)
      `(quote ,(λ (y)
                 `(quote ,(- x y))))))
  (define is-zero?
    (lambda (x)
      (if (= x 0)
          `(λ (x) (λ (y) x))
          `(λ (x) (λ (y) y)))))
  (define zero? is-zero?)

  ;; ideally, this would have the same source as the typeset
  ;; code in the paper:
  (define true (term (λ (x) (λ (y) x))))
  (define false (term (λ (x) (λ (y) y))))
  (define sel (term (λ (q) (λ (x) (λ (y) (((q x) y) '0))))))
  (define pair (term (λ (a) (λ (d) (λ (s) ((s a) d))))))
  (define empty (term ((,pair ,false) ,false)))
  (define cons (term (λ (a) (λ (d) ((,pair ,true) ((,pair a) d))))))
  (define foldn
    (term (λ (N)
            (λ (R)
              (λ (a)
                ((λ (f)
                   (((f f) a) N))
                 (λ (f)
                   (λ (a)
                     (λ (n)
                       (((,sel ((quote ,zero?) n))
                         (λ (d) a))
                        (λ (d)
                          (((f f) (R a)) (((quote ,minus) n) (quote 1)))))))))))))))

(module+ test
  (require (submod ".." library))

  (define-metafunction LC
    let : ([variable e]) e -> e
    [(let ([x e_rhs]) e_body)
     ((λ (x) e_body) e_rhs)])

  (define example
    (term (let ([D (λ (x1) (λ (x2) (λ (y) (λ (z) (x1 x2)))))])
            (let ([R ((D '1)
                      '2)])
              (((,foldn '2) (λ (a) ((,cons a) (R '0)))) ,empty)))))

  (check-true (redex-match? LC e true))
  (check-true (redex-match? LC e false))
  (check-true (redex-match? LC e sel))
  (check-true (redex-match? LC e pair))
  (check-true (redex-match? LC e cons))
  (check-true (redex-match? LC e foldn))
  (check-true (redex-match? LC e example))

  (check-equal? (run -->interp+gc example)
                (term (ctr obj 2))))

(require (only-in (submod "." library)
                  plus
                  minus
                  is-zero?
                  foldn))

(define (make-fib N)
  (term
   (λet fib (λ (fib)
              (λ (n)
                (((((quote ,is-zero?) n)
                   (λ (d) (quote 1)))
                  (λ (d)
                    (((((quote ,is-zero?) (((quote ,minus) n) (quote 1)))
                       (λ (d) (quote 1)))
                      (λ (d)
                        (((quote ,plus) ((fib fib) (((quote ,minus) n) (quote 1))))
                         ((fib fib) (((quote ,minus) n) (quote 2))))))
                     (quote 99))))
                 (quote 99))))
        ((fib fib) (quote ,N)))))

(module+ test
  (check-equal? (run -->interp+gc (term ((λ (x) x) (quote 42))))
                (term (ctr prim 42)))
  (check-equal? (run -->interp/sfs+gc (term ((λ (x) x) (quote 42))))
                (term (ctr prim 42)))

  (check-equal? (run -->interp+gc (term (((quote ,plus) (quote 42)) (quote 4))))
                (term (ctr prim 46)))
  (check-equal? (run -->interp+gc/sfs (term (((quote ,plus) (quote 42)) (quote 4))))
                (term (ctr prim 46)))

  (check-equal? (run-interp -->interp (make-fib 5))
                (term (ctr prim 8)))
  (check-equal? (run -->interp+gc (make-fib 5))
                (term (ctr prim 8)))
  (check-equal? (run-interp -->interp/sfs (make-fib 5))
                (term (ctr prim 8)))
  (check-equal? (run -->interp/sfs+gc (make-fib 5))
                (term (ctr prim 8)))
  (check-equal? (run -->interp+gc/sfs (make-fib 5))
                (term (ctr prim 8)))

  (void))
