#lang racket
(require "config.rkt"
         redex/pict
         pict)

(provide using-rewriters)

(define-syntax using-rewriters
  (syntax-rules ()
    [(using-rewriters #:xtup? xtup? e)
     (call-using-rewriters (lambda () e) #:xtup? xtup?)]
    [(using-rewriters e)
     (call-using-rewriters (lambda () e))]))

(define (italic str)
  (lambda ()
    (text str (non-terminal-style) (default-font-size))))

(define (call-using-rewriters thunk
                              #:xtup? [xtup? #t])
  (with-compound-rewriters
      (['tup rewrite-tuple]
       ['xtup (if xtup? rewrite-tuple (lambda (args) (list "" (list-ref args 2) "")))]
       ['ctr
        (lambda (args)
          (let ([args (drop-ws args)])
            (if (= (length args) 1)
                (append (list "⟨") args (list "⟩"))
                (append (list "⟨") (list (car args) " ") (add-between (cdr args) ", ") (list (italic-correct "⟩" (last args)))))))]
       ['set
        (lambda (args) (if (= (length args) 3)
                           (list (inset (text "∅") 0 -5 0 0))
                           (append (list "{") (add-between (drop-ws args) ", ") (list "}"))))]
       ['seq
        (lambda (args) (append (list "[") (add-between (drop-ws args) " ") (list "]")))]
       ['bnd
        (lambda (args) (append (list "") (add-between (drop-ws args) "=") (list "")))]
       ['free-vars rewrite-free-vars]
       ['free-vars-as-e rewrite-free-vars]
       ['element-of
        (lambda (args) (append (list "") (add-between (drop-ws args) " ∈ ") (list "")))]
       ['env-or
        (lambda (args) (append (list "") (add-between (drop-ws args) " ∨ ") (list "")))]
       ['fetch rewrite-lookup]
       ['store rewrite-assign]
       ['extend rewrite-assign]
       ['extend-end rewrite-assign]
       ['update rewrite-assign]
       ['prefix rewrite-append]
       ['suffix rewrite-append]
       ['kcons rewrite-cons]
       ['ksnoc rewrite-snoc]
       ['scons rewrite-cons]
       ['ssnoc rewrite-snoc]
       ['malloc (lambda (args) (list "⎸" (list-ref args 2) "⎹"))]
       ['find rewrite-lookup] ; actually returns only the address part of a tuple
       ['find* rewrite-lookup]
       ['forward rewrite-lookup] ; actually returns only the address part of an A entry
       ['forward* rewrite-lookup] ; actually returns only the address part of an A entry and matches tuple specially
       ['subtract (lambda (args) (list "" (list-ref args 2) "∖" (list-ref args 3) ""))]
       ['add (lambda (args) (list "" (list-ref args 2) " ∪ " (list-ref args 3) ""))]
       ['≥ (lambda (args) (list "" (list-ref args 2) " ≥ " (list-ref args 3) ""))]
       ['sub1 (lambda (args) (list "" (list-ref args 2) "−1"))]
       ['add1 (lambda (args) (list "" (list-ref args 2) "+1"))]
       ['len-Σ (lambda (args) (list "|" (list-ref args 2) (italic-correct "|" (list-ref args 2))))]
       ['∈dom (lambda (args) (list "" (list-ref args 2) " ∈ dom(" (list-ref args 3) ")"))]
       ['∉dom (lambda (args) (list "" (list-ref args 2) " ∉ dom(" (list-ref args 3) ")"))]
       ['∉ (lambda (args) (list "" (list-ref args 2) " ∉ " (list-ref args 3) ""))]
       ['-->interp-jf (make-rewrite-reduction '-->interp)]
       ['-->gc-jf (make-rewrite-reduction '-->gc)]
       ['-->combine-jf (make-rewrite-reduction '-->)]
       ['λet (lambda (args) (list ""
                                  (text "(let ([" (literal-style) (default-font-size))
                                  (list-ref args 2)
                                  " " 
                                  (list-ref args 3)
                                  "])"
                                  (struct-copy lw (list-ref args 4)
                                               [e "  "]
                                               [column-span 0]
                                               [line-span 0])
                                  (list-ref args 4)
                                  ")"))]
       ['close rewrite-metafunction]
       ['lookup rewrite-metafunction]
       ['bind rewrite-metafunction]
       ['retain-env rewrite-metafunction]
       ['retain-val rewrite-metafunction]
       ['retain rewrite-metafunction]
       ['skip-to rewrite-metafunction]
       ['kept-skip-to rewrite-metafunction]
       ['retain-skip-env rewrite-metafunction]
       ['retain-for rewrite-metafunction]       
       ['refine rewrite-metafunction]       
       ['horiz (lambda (args) (append (list "") (drop-ws args) (list "")))]
       ['vert (lambda (args) (append (list "") (drop-ws args) (list "")))])
    (with-atomic-rewriters
        (['variable-not-otherwise-mentioned (lambda () (text "variable" (non-terminal-style) (default-font-size)))]
         ['m/short (italic "m")]
         ['h (italic "config")]
         ['_N (italic "N")]
         ['_foldn (italic "foldn")]
         ['_true (italic "true")]
         ['_false (italic "false")]
         ['_sel (italic "sel")]
         ['_pair (italic "pair")]
         ['_empty (italic "empty")]
         ['_cons (italic "cons")]
         ['done (lambda () (text "[]" (default-style) (default-font-size)))])
      (parameterize ([default-style roman-font]
                     [literal-style sans-serif-font]
                     [metafunction-style sans-serif-font]
                     [label-style sans-serif-font]
                     [non-terminal-style `(italic . ,roman-font)]
                     [non-terminal-subscript-style `(subscript italic . ,roman-font)]
                     [label-font-size 10]
                     [label-space 10])
        (thunk)))))

(define (italic-correct str lw)
  (case (lw-e lw)
    [(Σ Σ_′ Σ_′′ S S_′ S_′′ K K_′ ρ_′)
     (inset (text str (default-style) (default-font-size)) 2 0 0 0)]
    [else
     str]))

(define (rewrite-tuple args)
  (define middle (add-between (drop-ws args) ", "))
  (append (list "" (just-before "⟨" (first middle)))
          middle
          (list (just-after (italic-correct "⟩" (last middle))
                            (last middle)) "")))

(define (rewrite-metafunction args)
  (define middle (add-between (drop-ws args) ", "))
  (append (list (hbl-append (text (format "~a" (lw-e (list-ref args 1))) (metafunction-style) (metafunction-font-size))
                            (text "⟦" (default-style) (default-font-size))))
          middle
          (list (italic-correct "⟧" (last middle)))))

(define (rewrite-lookup args)
  (list "" (list-ref args 2) "(" (list-ref args 3) ")"))
(define (rewrite-assign args)
  (list "" (list-ref args 2) " + {" (list-ref args 3) (text "=" |Times New Roman|) (list-ref args 4) "}"))
(define (rewrite-free-vars args)
  (list (text "free-vars" (metafunction-style) (metafunction-font-size)) "⟦" (list-ref args 2) (italic-correct "⟧" (list-ref args 2))))
(define (rewrite-append args)
  (list "" (list-ref args 2) "+" (list-ref args 3) ""))

(define (rewrite-cons args)
  (list "" (list-ref args 2) " :: " (list-ref args 3) ""))
(define (rewrite-snoc args)
  (list "" (list-ref args 2) " ++ [" (list-ref args 3) "]"))

(define (make-rewrite-reduction arrow)
  (lambda (args)
    (list "" (list-ref args 2) (inset (arrow->pict arrow) 5 0) (list-ref args 3) "")))

(define (drop-ws args)
  (drop-right (drop args 2) 1))

(define (make-arrow subscript)
  (lambda ()
    (hbl-append (arrow->pict '-->)
                (text subscript '(subscript . roman)))))

(set-arrow-pict! '-->interp (make-arrow "E"))
(set-arrow-pict! '-->gc (make-arrow "G"))
(set-arrow-pict! '-->interp/sfs (make-arrow "E"))
(set-arrow-pict! '-->gc/sfs (make-arrow "G"))
