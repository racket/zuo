#lang racket
(require "measure-lib.rkt"
         "test-run.rkt"
         "test-measure.rkt"
         redex/reduction-semantics)
(module+ test (require rackunit))

(run-and-log
 "ids"
 i (in-inclusive-range 1 10)
 (for/fold ([expr (term ',0)])
           ([j (in-range i)])
   `((λ (x) x) ,expr)))

(define (intro-example size)
  (define body-app
    (for/fold ([expr (x 0)])
              ([i (in-range (- size 1))])
      `(,expr ,(x (+ i 1)))))
  (define fn
    (for/fold ([expr `(λ (y) (λ (z) ,body-app))])
              ([i (in-range size)])
      `(λ (,(x (- size i 1)))
         ,expr)))
  (define passed-arguments
    (for/fold ([expr fn])
              ([i (in-range size)])
      `(,expr ',i)))
  (define n-calls-to-f
    (for/fold ([expr '(f '0)])
              ([i (in-range (- size 1))])
      `(((λ (i1) (λ (i2) i2)) ,expr) (f ',(+ i 1)))))
  `((λ (f) ,n-calls-to-f) ,passed-arguments))

(define (intro-example-with-pairs size)
  (define body-app
    (for/fold ([expr (x 0)])
              ([i (in-range (- size 1))])
      `(,expr ,(x (+ i 1)))))
  (define fn
    (for/fold ([expr `(λ (y) (λ (z) ,body-app))])
              ([i (in-range size)])
      `(λ (,(x (- size i 1)))
         ,expr)))
  (define passed-arguments
    (for/fold ([expr fn])
              ([i (in-range size)])
      `(,expr ',i)))
  (define n-calls-to-f
    (for/fold ([expr '(f '0)])
              ([i (in-range (- size 1))])
      `(((λ (i1) (λ (i2) (λ (z) ((z i1) i2)))) ,expr) (f ',(+ i 1)))))
  `((λ (f) ,n-calls-to-f) ,passed-arguments))

;; Like `intro-example-with-pairs`, but more precisely the variant in the GC section
(define (gc-example size)
  (define body-app
    (for/fold ([expr (x 0)])
              ([i (in-range (- size 1))])
      `(,expr ,(x (+ i 1)))))
  (define fn
    (for/fold ([expr `(λ (y) (λ (z) ,body-app))])
              ([i (in-range size)])
      `(λ (,(x (- size i 1)))
         ,expr)))
  (define passed-arguments
    (for/fold ([expr 'D])
              ([i (in-range size)])
      `(,expr ',i)))  
  (define true `(λ (x) (λ (y) x)))
  (define false `(λ (x) (λ (y) y)))
  (define sel `(λ (q) (λ (x) (λ (y) (((q x) y) '0)))))
  (define pair `(λ (a) (λ (d) (λ (s) ((s a) d)))))
  (define empty `((,pair ,false) ,false))
  (define cons `(λ (a) (λ (d) ((,pair ,true) ((,pair a) d)))))
  (define foldn
    `(λ (N)
       (λ (R)
         (λ (a)
           ((λ (f)
              (((f f) a) N))
            (λ (f)
              (λ (a)
                (λ (n)
                  (((,sel ((quote ,is-zero?) n))
                    (λ (d) a))
                   (λ (d)
                     (((f f) (R a)) (((quote ,minus) n) (quote 1)))))))))))))
  `((λ (D) ((λ (R) (((,foldn ',size) (λ (a) ((,cons a) (R '0)))) ,empty))
            ,passed-arguments))
    ,fn))

(module+ test
  (check-equal? (intro-example 1) `((λ (f) (f '0)) ((λ (x0) (λ (y) (λ (z) x0))) '0)))
  (check-equal? (intro-example 2) `((λ (f) (((λ (i1) (λ (i2) i2)) (f '0)) (f '1)))
                                    (((λ (x0) (λ (x1) (λ (y) (λ (z) (x0 x1))))) '0) '1))))

(define (x i) (string->symbol (~a 'x i)))

(module+ test
  ;; regresssion test
  (for ([i (in-inclusive-range 1 5)])
    (define e (gc-example i))
    (apply-reduction-relation* -->interp+gc/sfs
                               (term [tup eval 0 (tup ,e mt) done (set)]))))

(run-and-log
 "intro-2"
 i (in-inclusive-range 1 25)
 (intro-example i))

;; This takes about 22 minutes on my machine; 17 minutes when stopping at 61 instead of 65
(run-and-log
 "intro-2b"
 i (in-inclusive-range 1 65 4)
 (intro-example-with-pairs i))

;; This takes about 65 minutes on my machine; 50 minutes when stopping at 61 instead of 65
(run-and-log
 "gc"
 i (in-inclusive-range 1 65 4)
 (gc-example i))

(run-and-log
 "fib"
 i (in-inclusive-range 1 6)
 (make-fib i))

(run-and-log
 "intro-1"
 i (in-inclusive-range 1 10)
 (stop-early (term ((λ (f) ((f f) (λ (x) x)))
                    (λ (f) (λ (u) ((f f) (λ (z) z))))))
             (* i 200)))

(run-and-log
 "build test-measure.rkt"
 i (in-inclusive-range 1 10)
 (build i))

(run-and-log
 "many-big-env test-measure.rkt"
 i (in-inclusive-range 1 5)
 (many-big-env i))


(module+ main
  (require "measure-lib.rkt")
  (fetch-plots "ids")
  (fetch-plots "intro-1")
  (fetch-plots "intro-2")
  (fetch-plots "intro-2b")
  (fetch-plots "gc")
  (fetch-plots "fib")
  (fetch-plots "build test-measure.rkt")
  (fetch-plots "many-big-env test-measure.rkt"))
