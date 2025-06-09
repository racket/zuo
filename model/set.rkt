#lang racket
(require redex/reduction-semantics
         "grammar.rkt")

(provide assoc-ref
         split
         extend
         extend-end
         update
         add
         subtract
         prefix
         suffix
         element-of
         ∈dom
         ∉dom
         ∉)

(define-metafunction LC
  assoc-ref : any any -> any
  [(assoc-ref any_l any_k)
   ,(let ([p (for/or ([b (in-list (cdr (term any_l)))])
               (and (equal? (term any_k) (cadr b))
                    (cdr b)))])
      (and p (cadr p)))])

(define-metafunction LC
  split : any any -> any
  [(split any_k any_l)
   ,(let loop ([l (cdr (term any_l))] [accum null])
      (and (pair? l)
           (if (equal? (term any_k) (cadar l))
               (list (reverse accum) (car l) (cdr l))
               (loop (cdr l) (cons (car l) accum)))))])

(define-metafunction LC
  extend : any any any -> any
  [(extend (set any ...) any_k any_v)
   (set (bnd any_k any_v) any ...)])

(define-metafunction LC
  extend-end : any any any -> any
  [(extend-end (set any ...) any_k any_v)
   (set any ... (bnd any_k any_v) )])

(define-metafunction LC
  update : any any any -> any
  [(update any_l any_k any_v)
   (set any_new ...)
   (where (any_new ...)
          ,(let loop ([l (cdr (term any_l))] [accum null])
             (and (pair? l)
                  (if (equal? (term any_k) (cadar l))
                      (append (reverse accum) (cons (term (bnd any_k any_v)) (cdr l)))
                      (loop (cdr l) (cons (car l) accum))))))])


(define-judgment-form LC
  #:mode (∈dom I I)
  [(where #t ,(and (term (assoc-ref any_l any_k)) #t))
   -----
   (∈dom any_k any_l)])

(define-judgment-form LC
  #:mode (∉dom I I)
  [(where #f (assoc-ref any_l any_k))
   -----
   (∉dom any_k any_l)])

(define-judgment-form LC
  #:mode (∉ I I)
  [(where #f ,(member (term any_k) (cdr (term any_l))))
   -----
   (∉ any_k any_l)])

(module+ test
  (test-judgment-holds (∉dom z (extend (set) x (xtup y (set)))))
  (test-judgment-holds (∉dom y (extend (set) x (xtup y (set)))))
  (test-equal (judgment-holds (∉dom x (extend (set) x (xtup y (set))))) #f))

(define-metafunction LC
  subtract : (set x ...) (set x) -> (set x ...)
  [(subtract (set x ...) (set x_bye))
   (set x_other ...)
   (where (x_other ...) ,(remove (term x_bye) (term (x ...))))])

(define-metafunction LC
  add : any (set any) -> any
  [(add any (set any_v))
   any
   (side-condition (member (term any_v) (cdr (term any))))]
  [(add (set any ...) (set any_v))
   (set any ... any_v)])

(define-metafunction LC
  prefix : any any -> any
  [(prefix any_e (seq any ...))
   (seq any_e any ...)])

(define-metafunction LC
  suffix : any any -> any
  [(suffix (seq any ...) any_e)
   (seq any ... any_e)])

(define-metafunction LC
  element-of : any (set any ...) -> any
  [(element-of any (set any_1 ...))
   ,(member (term any) (term (any_1 ...)))])
