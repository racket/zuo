#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "store.rkt"
         "set.rkt")

(provide skip-to
         retain-skip-env)

;; Given an environment and free variables, returns
;;   * the largest subevironment that either binds one
;;     of the free variables or is known to be retained;
;;     this result represents an environment frame that
;;     is retained
;;   * the largest subevironment that binds one of the
;;     free variables; this result represents an environment
;;     frame that needs to be swept
(define-metafunction LC
  skip-to : ρ (set x ...)  Σ -> ρ
  [(skip-to mt (set) Σ)
   mt]
  [(skip-to σ (set x_live ...) Σ)
   σ
   (where (ctr env (tup x v ρ_next)) (fetch Σ σ))
   (side-condition (term (element-of x (set x_live ...))))]
  [(skip-to σ (set x_live ...) Σ)
   (skip-to ρ_next (set x_live ...) Σ)
   (where (ctr env (tup x v ρ_next)) (fetch Σ σ))
   (judgment-holds (∉ x (set x_live ...)))])

;; Similar to `retain`, but in terms of `ρ` instead of `σ`,
;; and skips ahead in `ρ` to a subenvironment that binds
;; one of the live `x`s. The result is two environments,
;; instead of just one:
;;  * an environment that is either already retained (via
;;    some other live-variable set) or that binds a live `x`;
;;    this is the environment that should be referenced in
;;    place of `ρ`
;;  * an environment that binds a live `x`; this is the
;;    environment where sweeping should continue, and it
;;    is allocated in the result `A` and added to the sweep
;;    stack in the result `S`
;; Note that `retain-env` via `retain` only adds to `S` if the
;; retained environment is newly alocated, but `retain-skip-env`
;; via `retain-for` adds to `S` when the live `x` set is
;; different than any before.
;; If an enviornment is represented as a binary tree instead
;; of a linked list (see `lookup`), and if the set of free
;; variables is similarly a binary tree, then we can perform
;; the skip in O(log N) time.
(define-metafunction LC
  retain-skip-env : ρ (set x ...) Σ A Σ S -> (tup ρ ρ A Σ S)
  [(retain-skip-env mt (set) Σ_from A Σ_to S)
   (tup mt mt A Σ_to S)]
  [(retain-skip-env σ_from (set) Σ_from A Σ_to S) ; no `x` means second result must be `mt`
   (tup ρ_kept-to mt A Σ_to S)
   (where (tup ρ_kept-from ρ_prev mt) (kept-skip-to σ_from (set) Σ_from A))
   (where ρ_kept-to (forward A ρ_kept-from))]
  [(retain-skip-env σ (set x ...) Σ_from A Σ_to S) ; some `x` means that we'll return addresses
   (tup σ_kept-to σ_live-to A_′ Σ_to′ S_′)
   (where (tup σ_kept-from ρ_prev σ_live-from) (kept-skip-to σ (set x ...) Σ_from A)) ; jump to environment extension that overlaps with `x`s
   (where (tup σ_live-to A_′ S_′) (retain-for σ_live-from (set x ...) A S))  ; allocate in to-space, if not already allocated
   (where Σ_to′ (refine ρ_prev σ_live-to A_′ Σ_to))
   (where σ_kept-to (find A_′ σ_kept-from))])

(define-metafunction LC
  refine : ρ σ A Σ -> Σ
  [(refine mt σ_to A Σ)
   Σ]
  [(refine σ_prev-from σ_to A Σ)
   Σ
   (judgment-holds (∉dom (find A σ_prev-from) Σ))]
  [(refine σ_prev-from σ_to A Σ)
   (store Σ σ_prev-to (ctr env (tup x v σ_to)))
   (where σ_prev-to (find A σ_prev-from))
   (where (ctr env (tup x v ρ)) (fetch Σ σ_prev-to))])

(define-metafunction LC
  retain-for : σ (set x ...) A S -> (tup σ A S)
  [(retain-for σ_from (set x ...) A S)
   (tup σ_to A S)
   (where (xtup σ_to L) (find* A σ_from))
   (side-condition (term (element-of (set x ...) L)))]
  [(retain-for σ_from (set x ...) A S)
   (tup σ_to (update A σ_from (xtup σ_to (add L (set (set x ...))))) (ssnoc S (xtup σ_from (set x ...))))
   (where (xtup σ_to L) (find* A σ_from))]
  [(retain-for σ_from (set x ...) A S)
   (tup σ_to (extend A σ_from (xtup σ_to (set (set x ...)))) (ssnoc S (xtup σ_from (set x ...))))
   (judgment-holds (∉dom σ_from A))
   (where σ_to (malloc A))])

;; Given an environment and free variables, returns
;;   * the largest subevironment that either binds one
;;     of the free variables or is known to be retained;
;;     this result represents an environment frame that
;;     is retained
;;   * an optional subevironment that may need to be
;;     updated to point to the newly allocated environment
;;     for the third result; this is `mt` if the first and
;;     third results are the same, otherwise it's a 
;;     subenvironment of the first result and the immediate
;;     superenvironment of the third result
;;   * the largest subevironment that binds one of the
;;     free variables; this result represents an environment
;;     frame that needs to be swept
(define-metafunction LC
  kept-skip-to : ρ (set x ...)  Σ A -> (tup ρ ρ ρ)
  [(kept-skip-to mt (set) Σ A)
   (tup mt mt mt)]
  [(kept-skip-to σ (set x_live ...) Σ A)
   (tup σ mt σ)
   (where (ctr env (tup x v ρ_next)) (fetch Σ σ))
   (side-condition (term (element-of x (set x_live ...))))]
  [(kept-skip-to σ (set x_live ...) Σ A)
   (kept-skip-to ρ_next (set x_live ...) Σ A)
   (judgment-holds (∉dom σ A)) ; not allocated
   (where (ctr env (tup x v ρ_next)) (fetch Σ σ))
   (judgment-holds (∉ x (set x_live ...)))]
  [(kept-skip-to σ (set x_live ...) Σ A)
   (tup σ (env-or ρ_prev σ) ρ_live)
   ;; `σ` is allocated, but does not bind an `x` in `x_live`
   (judgment-holds (∈dom σ A)) ; allocated
   (where (ctr env (tup x v ρ_next)) (fetch Σ σ))
   (judgment-holds (∉ x (set x_live ...)))
   (where (tup ρ_kept ρ_prev ρ_live) (kept-skip-to ρ_next (set x_live ...) Σ A))])

(define-metafunction LC
  env-or : ρ ρ -> ρ
  [(env-or mt ρ) ρ]
  [(env-or σ ρ) σ])

(module+ pict
  (require "config.rkt"
           redex/pict
           "typeset.rkt")
  (provide (all-defined-out))
  (define-values (retain-skip-env-pict
                  skip-to-pict
                  retain-for-pict
                  kept-skip-to-pict
                  refine-pict
                  env-or-pict)
    (using-rewriters
     (values (parameterize ([metafunction-pict-style 'left-right/compact-side-conditions]
                            [metafunction-fill-acceptable-width 800])
               (render-metafunction retain-skip-env))
             (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
               (render-metafunction skip-to))
             (render-metafunction retain-for)
             (render-metafunction kept-skip-to)
             (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
               (render-metafunction refine))
             (render-metafunction env-or)))))

(module+ test
  (require rackunit)
  (define store1 (term (set (bnd 0 (ctr env (tup n (ctr prim 2) 1))) (bnd 1 (ctr env (tup fib (ctr prim 0) mt))))))
  (check-equal? (term (kept-skip-to mt (set) ,store1 (set (bnd 0 (xtup 0 (set))))))
                (term (tup mt mt mt)))
  (check-equal? (term (kept-skip-to 0 (set) ,store1 (set (bnd 0 (xtup 0 (set))))))
                (term (tup 0 0 mt)))
  (check-equal? (term (kept-skip-to 0 (set fib) ,store1 (set (bnd 0 (xtup 0 (set))))))
                (term (tup 0 0 1)))
  (check-equal? (term (kept-skip-to 0 (set n) ,store1 (set (bnd 0 (xtup 0 (set))))))
                (term (tup 0 mt 0)))
  (check-equal? (term (kept-skip-to 0 (set n fib) ,store1 (set (bnd 0 (xtup 0 (set))))))
                (term (tup 0 mt 0)))
  (check-equal? (term (kept-skip-to 0 (set n) ,store1 (set)))
                (term (tup 0 mt 0)))
  (check-equal? (term (kept-skip-to 0 (set fib) ,store1 (set)))
                (term (tup 1 mt 1)))
  (check-equal? (term (kept-skip-to 0 (set) ,store1 (set (bnd 1 (xtup 0 (set))))))
                (term (tup 1 1 mt)))

  (require (submod ".." pict))
  retain-skip-env-pict
  skip-to-pict
  retain-for-pict
  kept-skip-to-pict
  refine-pict
  env-or-pict)
