#lang racket/base

(require rackunit)


(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(check-false (atom? '()))

(provide atom?)