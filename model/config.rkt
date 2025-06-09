#lang racket

(require redex/pict racket/gui/base)

(provide roman-font
         sans-serif-font
         |Times New Roman|)

(define (pick-from which font1 . fonts)
  (define all-fonts (get-face-list))
  (define found-it
    (for/or ([font (in-list (cons font1 fonts))])
      (and (member font all-fonts)
           font)))
  (cond
    [found-it found-it]
    [else
     (eprintf "warning: could not find the ~a font\n" which)
     font1]))

(define roman-font
  (pick-from 'roman-font "Linux Libertine O"))
(define sans-serif-font
  (pick-from 'sans-serif-font
             "Linux Biolinum"
             "Linux Biolinum O"))
(define |Times New Roman| (pick-from 'times-new-roman "Times New Roman"))

;; "Inconsolata"
