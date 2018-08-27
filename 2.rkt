#lang racket

(require rackunit)
(require "./1.rkt")

(define (lat? l)
  (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]))

(check-true (lat? '(bacon and eggs)))
(check-false (lat? '(bacon (and eggs))))


(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else
     (or (eq? (car lat) a)
         (member? a (cdr lat)))]))

(check-true (member? 'meat '(mashed potatoes and meat gravy)))
(check-false (member? 'liver '(bagels and lox)))
                                   