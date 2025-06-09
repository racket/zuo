#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "set.rkt")

(provide fetch
         store
         lookup
         bind
         close
         retain-env
         retain-val
         retain
         find
         find*
         forward
         forward*
         malloc
         ksnoc
         ssnoc)

;; Fetch a binding from the store, which is constant-time
;; in a practical implementation.
(define-metafunction LC
  fetch : Σ σ -> b
  [(fetch Σ σ)
   (assoc-ref Σ σ)])

;; Add or replace a binding in the store.
(define-metafunction LC
  store : Σ σ b -> Σ
  [(store Σ σ b)
   (set any_1 ... (bnd σ b) any_2 ...)
   (where ((any_1 ...) (bnd σ b_old) (any_2 ...)) (split σ Σ))]
  [(store (set any_1 ...) σ b)
   (set any_1 ... (bnd σ b))])

;; Treating lookup as constant-time, where an implementation could
;; use a binary tree, especially if variables are replaced by binding
;; depths (and treating O(N log N) as close enough to O(N)).
(define-metafunction LC
  lookup : x ρ Σ -> v
  [(lookup x σ Σ)
   v
   (where (ctr env (tup x v ρ)) (fetch Σ σ))]
  [(lookup x σ Σ)
   (lookup x σ_next Σ)
   (where (ctr env (tup x_other v_other σ_next)) (fetch Σ σ))])

;; Extend an environment by allocating.
(define-metafunction LC
  bind : x v ρ Σ -> (tup ρ Σ)
  [(bind x v ρ Σ)
   (tup σ_′ (extend Σ σ_′ (ctr env (tup x v ρ))))
   (where σ_′ (malloc Σ))])

;; Extend an environment by allocating.
(define-metafunction LC
  close : e ρ Σ -> (tup ρ Σ)
  [(close e ρ Σ)
   (tup σ_′ (extend Σ σ_′ (ctr clos (tup e ρ))))
   (where σ_′ (malloc Σ))])

(define-metafunction LC
  malloc : any -> σ
  [(malloc any)
   ,(length (cdr (term any)))])

;; The "retain" GC step checks whether ρ has already been
;; allocated in new space, and it returns that address if
;; so. Otherwise, allocates a new address, but doesn't copy
;; anything out of from-space, yet; instead, adds it to a
;; list of from-space addresses that are allocated an will
;; need to be copied. An object is swept at the same time
;; that it is copied. This is abstractly the same as a usual
;; 2-space collector, just representing the reached-but-not-swept
;; objects a little differently.
(define-metafunction LC
  retain-env : ρ A S -> (tup ρ A S)
  [(retain-env mt A S)
   (tup mt A S)]
  [(retain-env σ_from A S)
   (retain σ_from A S)])

(define-metafunction LC
  retain-val : v A S -> (tup v A S)
  [(retain-val (ctr prim lit) A S)
   (tup (ctr prim lit) A S)]
  [(retain-val (ctr obj σ_from) A S)
   (tup (ctr obj σ_to) A_′ S_′)
   (where (tup σ_to A_′ S_′) (retain σ_from A S))])

(define-metafunction LC
  retain : σ A S -> (tup σ A S)
  [(retain σ_from A S)
   (tup σ_to A S)
   (where σ_to (forward A σ_from))]
  [(retain σ_from A S)
   (tup σ_to (extend A σ_from (xtup σ_to (set))) (ssnoc S (xtup σ_from (set))))
   (judgment-holds (∉dom σ_from A))
   (where σ_to (malloc A))])

;; Get the to-space address for a from-space address
;; that has been allocated. In a realistic implementation,
;; this is getting an immediate forwarding pointer, so
;; we treat it as constant-time.
(define-metafunction LC
  find : A σ -> σ
  [(find A σ_from)
   σ_′
   (where (xtup σ_′ L) (assoc-ref A σ_from))])

;; Maybe finds, returns full tuple
(define-metafunction LC
  find* : A ρ -> any
  [(find* A σ_from)
   (assoc-ref A σ_from)])

;; Like `find`, but works on `mt`, and returns `mt` for
;; a not-yet-forwarded store address.
(define-metafunction LC
  forward : A ρ -> ρ
  [(forward A σ_from)
   σ_to
   (where (xtup σ_to L) (assoc-ref A σ_from))]
  [(forward A ρ)
   mt])

;; Like `forward`, but only when `(x ...)` is a previously
;; added live set
(define-metafunction LC
  forward* : A (tup σ (set x ...)) -> ρ
  [(forward* A (tup σ_from (set x ...)))
   σ_′
   (where (xtup σ_′ L) (assoc-ref A σ_from))
   (side-condition (member (term (set x ...)) (cdr (term L))))]
  [(forward* A (tup σ_from (set x ...)))
   mt])

(define-metafunction LC
  ksnoc : K k -> K
  [(ksnoc done k) (kcons k done)]
  [(ksnoc (kcons k_′ K) k) (kcons k_′ (ksnoc K k))])

(define-metafunction LC
  ssnoc : S any -> S
  [(ssnoc (seq) any) (scons any (seq))]
  [(ssnoc (scons any_′ S) any) (scons any_′ (ssnoc S any))])

(module+ pict
  (require "config.rkt"
           redex/pict
           "typeset.rkt")
  (provide (all-defined-out))
  (define-values (lookup-pict
                  bind-pict
                  close-pict
                  retain-env-pict
                  retain-val-pict
                  retain-pict
                  A-of-sigma-from-pict)
    (using-rewriters
     #:xtup? #f
     (values (render-metafunction lookup)
             (render-metafunction bind)
             (render-metafunction close)
             (render-metafunction retain-env)
             (render-metafunction retain-val)
             (render-metafunction retain)
             (render-term LC (find A σ_from))))))

(module+ test
  (require (submod ".." pict))
  lookup-pict
  bind-pict
  close-pict
  retain-env-pict
  retain-val-pict
  retain-pict
  A-of-sigma-from-pict)
