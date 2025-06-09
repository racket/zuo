#lang racket
(require redex/reduction-semantics
         "grammar.rkt"
         "test-run.rkt"
         plot/pict
         pict
         racket/runtime-path
         (for-syntax syntax/parse))
(provide run-and-log
         fetch-plots fetch-mem-plot fetch-time-plot
         (struct-out steps) (struct-out stop-early)
         (contract-out
          [count-steps (->* ((or/c reduction-relation? IO-judgment-form?)
                             any/c)
                            (#:stop-early (or/c #f natural?))
                            steps?)]))

(define-syntax (run-and-log stx)
  (syntax-parse stx
    [(_ name i range
        expr1 expr ...)
     #'(run-and-log/proc
        name
        (λ (i) expr1 expr ...)
        range)]))

(define-runtime-path steps-data "steps-data")
(unless (directory-exists? steps-data) (make-directory steps-data))

(define (run-and-log/proc name build-term range)
  (unless (string? name) (error 'run-and-log "name should be a string\n  got: ~e" name))
  (unless (file-exists? (name->filename name))
    (printf "running ~a:" name) (flush-output)
    (define start-msec (current-process-milliseconds))
    (define data
      (for/list ([--> (in-list (list -->interp+gc
                                     -->interp/sfs+gc
                                     -->interp+gc/sfs))]
                 [-->name (in-list '(-->interp+gc
                                     -->interp/sfs+gc
                                     -->interp+gc/sfs))])
        (printf " ~a ..." -->name)
        (flush-output)
        (cons -->name
              (for/list ([i range])
                (define _ith-term (build-term i))
                (define-values (ith-term stop-early value-for-i)
                  (cond
                    [(stop-early? _ith-term)
                     (values (stop-early-term _ith-term)
                             (stop-early-steps _ith-term)
                             (stop-early-steps _ith-term))]
                    [else
                     (values _ith-term #f i)]))
                (unless (e? ith-term)
                  (error 'run-and-log "term is not an e\n  size: ~a\n  term: ~s" i ith-term))
                (cons value-for-i (count-steps --> ith-term #:stop-early stop-early))))))
    (printf " done\n")
    (printf "   took ~a seconds\n" (~r #:precision 1 (/ (- (current-process-milliseconds) start-msec) 1000)))
    (call-with-output-file (name->filename name)
      (λ (port)
        (pretty-write data port)))))

(struct stop-early (term steps))

(define e? (redex-match LC e))

(define (fetch-plots name #:title [title name] #:xlabel [xlabel "Expression Size"])
  (hc-append
   10
   (fetch-time-plot name #:title title #:xlabel xlabel)
   (fetch-time-plot name #:title title #:xlabel xlabel #:gc-time? #f)
   (fetch-mem-plot name #:title title #:xlabel xlabel)))

(define (fetch-time-plot name #:title [title name] #:xlabel [xlabel "Expression Size"]
                         #:name2label [name->label -->name->label]
                         #:gc-time? [gc-time? #t])
  (unless (file-exists? (name->filename name))
    (error 'fetch-plot "data for ~a missing" name))
  (define data (call-with-input-file (name->filename name) read))
  (plot
   #:y-min 0
   #:title (if title (~a title ", Runtime") #f)
   #:y-label (if gc-time? "Evaluation Steps (including GC)" "Evaluation Steps (not counting GC)")
   #:x-label xlabel
   #:width plot-width
   #:height plot-height
   (for/list ([-->name+one-red-data (in-list data)])
     (match-define (cons -->name one-red-data) -->name+one-red-data)
     (define pts
       (for/list ([five-tuple (in-list one-red-data)])
         (match-define (cons i (steps evals num-gcs gcs peak-memory-use)) five-tuple)
         (vector i (+ evals (if gc-time? gcs 0)))))
     (points
      #:label (~a (name->label -->name))
      #:color (-->name->color -->name)
      #:sym (-->name->sym -->name)
      #:x-max (pick-max pts 0)
      #:y-max (pick-max pts 1)
      pts))))

(define (fetch-mem-plot name #:title [title name] #:xlabel [xlabel "Expression Size"]
                        #:name2label [name->label -->name->label])
  (unless (file-exists? (name->filename name))
    (error 'fetch-plot "data for ~a missing" name))
  (define data (call-with-input-file (name->filename name) read))
  (plot
   #:y-min 0
   #:title (if title (~a title ", Peak Memory Use") #f)
   #:y-label "Peak Store Size"
   #:x-label xlabel
   #:width plot-width
   #:height plot-height
   (for/list ([-->name+one-red-data (in-list data)])
     (match-define (cons -->name one-red-data) -->name+one-red-data)
     (define pts
       (for/list ([five-tuple (in-list one-red-data)])
        (match-define (cons i (steps evals num-gcs gcs peak-memory-use)) five-tuple)
         (vector i peak-memory-use)))
     (points
      #:label (~a (name->label -->name))
      #:color (-->name->color -->name)
      #:sym (-->name->sym -->name)
      #:x-max (pick-max pts 0)
      #:y-max (pick-max pts 1)
      pts))))

;; inset rightpoint point by 5% to make it easier to see
(define (pick-max pts idx)  
  (define v-max
    (for/fold ([v-max 0]) ([pt (in-list pts)])
      (max (vector-ref pt idx) v-max)))
  (* v-max 1.05))

;; this uses `subtract-adjacent` to approximate the idea of a
;; derivative to see if the curves flatten out to constants after
;; what we hope their degree is. It doesn't work out currently,
;; but that might be because the fuel introduces some noise
#|
> (kind-of-derivative "intro-2b" '-->interp/sfs+gc 2)
'(1 -1 8 2 -4 5 1 2 9 9 -12 3 -1 15 -4 -3 -13 19)
> (kind-of-derivative "intro-2b" '-->interp+gc/sfs 1)
'(5 7 6 6 5 9 4 5 10 1 5 7 8 8 7 -1 7 6 4)
|#
(define (kind-of-derivative name desired--> times)
  (unless (file-exists? (name->filename name))
    (error 'fetch-plot "data for ~a missing" name))
  (define data (call-with-input-file (name->filename name) read))
  (for/or ([-->name+one-red-data (in-list data)])
    (match-define (cons -->name one-red-data) -->name+one-red-data)
    (cond
      [(equal? desired--> -->name)
       (define peak-memory-uses
         (for/list ([five-tuple (in-list one-red-data)])
           (match-define (cons i (steps evals num-gcs gcs peak-memory-use)) five-tuple)
           peak-memory-use))
       (for/fold ([data peak-memory-uses])
                 ([i (in-range times)])
         (subtract-adjacent data))]
      [else #f])))

(define (subtract-adjacent data)
  (for/list ([datum1 (in-list data)]
             [datum2 (in-list (cdr data))])
    (- datum2 datum1)))

(define plot-width 300)
(define plot-height 300)

(define (-->name->label -->name)
  (match -->name
    ['-->interp+gc "Fig. 5"]
    ['-->interp/sfs+gc "Figs. 6 & 7"]
    ['-->interp+gc/sfs "Fig. 8"]))
  
(define (-->name->color -->name)
  (match -->name
    ['-->interp+gc "black"]
    ['-->interp/sfs+gc "firebrick"]
    ['-->interp+gc/sfs "forestgreen"]))

(define (-->name->sym -->name)
  (match -->name
    ['-->interp+gc 'fullcircle]
    ['-->interp/sfs+gc 'plus]
    ['-->interp+gc/sfs '5star]))
                        

(define (name->filename name) (build-path steps-data (~a name ".rktd")))

;; red expr -> steps
;;   if stop-early is a nat, then stop after that many steps
(define (count-steps -->interp+gc e-term #:stop-early [stop-early #f])
  (let loop ([state (term [tup eval 0 (tup ,e-term mt) done (set)])]
             [stop-early stop-early]
             [evals 0]
             [num-gcs 0]
             [gc-sizes null]
             [gc-durations null]
             [gcs 0]
             [gc-start #f]
             [peak-memory-use 0])
    ; (pretty-print state)
    (define pre-gc? (gc-state? state))
    (define nexts (apply-reduction-relation -->interp+gc state))
    (cond
      [(or (null? nexts) (equal? 0 stop-early))
       (when (and (not stop-early) (gc-state? state)) (error 'measure-lib.rkt "should not end in GC state!"))
       (steps evals num-gcs #;(reverse gc-sizes) #;(reverse gc-durations) gcs
             (max peak-memory-use (if (gc-state? state)
                                      0
                                      (memory-use state))))]
      [(pair? (cdr nexts))
       (error 'measure-lib.rkt "not deterministic")]
      [else
       (define new-state (car nexts))
       (define post-gc? (gc-state? new-state))
       (define gc? (or pre-gc? post-gc?)) ; count GC steps and transitions as GC
       (loop new-state
             (and stop-early (- stop-early 1))
             (if gc? evals (+ evals 1))
             (if (and post-gc? (not pre-gc?)) (+ num-gcs 1) num-gcs)
             (if (and pre-gc? (not post-gc?)) (cons (length (list-ref state 4)) gc-sizes) gc-sizes)
             (if (and pre-gc? (not post-gc?)) (cons (- gcs gc-start) gc-durations) gc-durations)
             (if gc? (+ gcs 1) gcs)
             (if (not pre-gc?) gcs gc-start)
             (if (and post-gc? (not pre-gc?))
                 (max peak-memory-use (memory-use state))
                 peak-memory-use))])))

;; evals : natural? -- number of steps of evaluation
;; num-gcs : natural? -- number of gcs
;; gcs : natural? -- number of steps in gc
;; peak-memory-use : natural? -- size of the largest store during execution
(struct steps (evals num-gcs gcs peak-memory-use) #:prefab)

(define gc-state? (redex-match? LC [tup gc any ...]))
(define mem-match (redex-match LC [tup eval any_steps m K (set any_store ...)]))

(define (memory-use state)
  (define mtch (mem-match state))
  (unless (and (list? mtch) (= 1 (length mtch)))
    (pretty-write state)
    (pretty-write mtch)
    (error 'memory-use "state didn't match pattern"))
  (for/or ([bind (in-list (match-bindings (car mtch)))])
    (and (equal? (bind-name bind) 'any_store)
         (length (bind-exp bind)))))
