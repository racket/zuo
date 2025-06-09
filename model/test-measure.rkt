#lang racket
(require redex/reduction-semantics
         rackunit
         "grammar.rkt"
         "test-run.rkt"
         "measure-lib.rkt"
         (for-syntax syntax/parse))
(provide build many-big-env)

(define (build n [make-body (lambda (vars) vars)])
  (let loop ([n n] [vars '(quote 0)])
    (cond
      [(zero? n)
       (term (λ (d) ,(make-body vars)))]
      [else
       (define x (gensym 'x))
       `((λ (,x)
           ,(loop (sub1 n) (term (,x ,vars))))
         (quote 0))])))

(define (many-big-env i)
  (term
   (,(build
      i
      (λ (vars)
        (term ((λ (loop)
                 ((loop loop) (quote 15)))
               (λ (loop)
                 (λ (n)
                   (((((quote ,is-zero?) n)
                      (λ (d)
                        (λ (x) x)))
                     (λ (d)
                       ((λ (v) (λ (d) ,vars))
                        ((loop loop) (((quote ,minus) n) (quote 1))))))
                    (quote 99))))))))
    (quote 99))))

(module+ main
  (for/list ([i (in-range 30)])
    (count-steps -->interp+gc (build i)))
  (for/list ([i (in-range 16)])
    (count-steps -->interp/sfs+gc (build i)))
  ;; Beware: step count grows linearly, but Redex model takes quadratic time!
  (for/list ([i (in-range 30)])
    (count-steps -->interp+gc/sfs (build i)))

  (count-steps -->interp+gc (make-fib 5))
  (count-steps -->interp/sfs+gc (make-fib 5))
  (count-steps -->interp+gc/sfs (make-fib 5))

  (count-steps -->interp+gc (many-big-env 10))
  (count-steps -->interp/sfs+gc (many-big-env 10))
  (count-steps -->interp+gc/sfs (many-big-env 10)))