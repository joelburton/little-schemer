#lang racket

(require rackunit)
(require "1.rkt")
(provide operator 1st-sub-exp 2nd-sub-exp)


; numbered doesn't just look for "all numbers and math symbols",
; but (number|aexp symbol number|aexp)"  [this isn't super-clear
; in book at first

(define (numbered?-v1 aexp)
  (cond
    [(atom? aexp) (number? aexp)]
    [(eq? (car (cdr aexp)) '+)
     (and (numbered?-v1 (car aexp))
          (numbered?-v1 (car (cdr (cdr aexp)))))]
    [(eq? (car (cdr aexp)) '×)
     (and (numbered?-v1 (car aexp))
          (numbered?-v1 (car (cdr (cdr aexp)))))]
    [(eq? (car (cdr aexp)) '↑)
     (and (numbered?-v1 (car aexp))
          (numbered?-v1 (car (cdr (cdr aexp)))))]))

(define (numbered? aexp)
  (cond
    [(atom? aexp) (number? aexp)]
    [else
     (and (numbered? (car aexp))
          (numbered? (car (cdr (cdr aexp)))))]))

(check-true (numbered? 1))
(check-true (numbered? '(3 + (4 ↑ 5))))
(check-false (numbered? '(2 × sausage)))


(define (value nexp)
  (cond
    [(atom? nexp) nexp]
    [(eq? (car (cdr nexp)) '+)
     (+ (value (car nexp)) (value (car (cdr (cdr nexp)))))]
    [(eq? (car (cdr nexp)) '×)
     (* (value (car nexp)) (value (car (cdr (cdr nexp)))))]
    [else ; i would prefer (eq? (car (cdr nexp)) '↑)
     (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))]))
    
(check-eq? (value 13) 13)
(check-eq? (value '(1 + 3)) 4)
(check-eq? (value '(1 + (3 ↑ 4))) 82)

; wrong version of value for exprs like (+ 1 3)

(define (value-wrong nexp)
  (cond
    [(atom? nexp) nexp]
    [(eq? (car nexp) '+)
     (+ (value-wrong (cdr nexp))
        (value-wrong (cdr (cdr nexp))))]
    [(eq? (car nexp) '×)
     (* (value-wrong (cdr nexp))
        (value-wrong (cdr (cdr nexp))))]
    [else  ; i prefer (eq? (car nexp) '↑)
     (expt (value-wrong (cdr nexp))
           (value-wrong (cdr (cdr nexp))))]))

; this should fail
(check-exn exn:fail? (λ () (value-wrong '(+ 1 3))))


(define (1st-sub-exp-v1 aexp)
  (cond
    [else (car (cdr aexp))]))

; simplified

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

; right version of (+ 1 2)

(define (value-prefix nexp)
  (cond
    [(atom? nexp) nexp]
    [(eq? (operator nexp) '+)
     (+ (value-prefix (1st-sub-exp nexp))
        (value-prefix (2nd-sub-exp nexp)))]
    [(eq? (car nexp) '×)
     (* (value-prefix (1st-sub-exp nexp))
        (value-prefix (2nd-sub-exp nexp)))]
    [else  ; i prefer (eq? (car nexp) '↑)
     (expt (value-prefix (1st-sub-exp nexp))
           (value-prefix (2nd-sub-exp nexp)))]))

(check-eq? (value-prefix '1) 1)
(check-eq? (value-prefix '(+ 1 2)) 3)
(check-eq? (value-prefix '(× (+ 1 2) (+ 2 1))) 9)


(define (sero? n)
  (null? n))

(define (edd1 n)
  (cons '() n))

(define (zub1 n)
  (cdr n))

(define (zplus n m)
  (cond
    [(sero? m) n]
    [else (edd1 (zplus n (zub1 m)))]))

(check-equal? (zplus '() '()) '())  ; 0 + 0 = 0  
(check-equal? (zplus '() '(())) '(()))  ; 0 + 1 = 1
(check-equal? (zplus '(()) '()) '(()))  ; 1 + 0 = 1
(check-equal? (zplus '(()) '(())) '(() ()))  ; 1 + 1 = 2
