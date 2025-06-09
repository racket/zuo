#lang racket
(require redex/reduction-semantics
         "grammar.rkt")

(provide free-vars
         free-vars-as-e)

;; Treating a free-variable calculation as constant-time,
;; since it could be computed once and stored alongside
;; the original program's terms.
(define-metafunction LC
  free-vars* : e (set x ...) (set x ...) -> (set x ...)
  [(free-vars* x (set x_bound ...) (set x_free ...))
   (set x_free ...)
   (side-condition (member (term x) (term (x_bound ... x_free ...))))]
  [(free-vars* x (set x_bound ...) (set x_free ...))
   (set x_free ... x)
   (side-condition (not (member (term x) (term (x_bound ... x_free ...)))))]
  [(free-vars* (quote any) any_bound any_free)
   any_free]
  [(free-vars* (e_fun e_arg) any_bound any_free)
   (free-vars* e_fun any_bound any_new-free)
   (where any_new-free (free-vars* e_arg any_bound any_free))]
  [(free-vars* (λ (x) e) (set x_bound ...) any_free)
   (free-vars* e (set x x_bound ...) any_free)])

(define-metafunction LC
  free-vars : e -> (set x ...)
  [(free-vars e)
   (free-vars* e (set) (set))])

;; For GC interaction, it's convenient to represent a
;; set of free variables as an expression
(define-metafunction LC
  free-vars-as-e : e -> e
  [(free-vars-as-e e)
   (vars-as-e any_free (quote 0))
   (where any_free (free-vars* e (set) (set)))])

(define-metafunction LC
  vars-as-e : (set x ...) e -> e
  [(vars-as-e (set) e)
   e]
  [(vars-as-e (set x x_rest ...) e)
   (vars-as-e (set x_rest ...) (x e))])
