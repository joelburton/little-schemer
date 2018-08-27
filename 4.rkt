#lang racket

(require rackunit)

(define (•+ n m)
  (cond
    [(zero? m) n]
    [else (•+ (add1 n) (sub1 m))]))

(check-eq? (•+ 46 12) 58)

(define (•- n m)
  (cond
    [(zero? m) n]
    [else (sub1 (•- n (sub1 m)))]))

(check-eq? (•- 14 3) 11)
(check-eq? (•- 17 9) 8)


(define (addtup tup)
  (cond
    [(null? tup) 0]
    [else (+ (car tup) (addtup (cdr tup)))]))

(check-eq? (addtup '(1 2 3 4)) 10)


(define (× n m)
  (cond
    [(zero? m) 0]
    [else (+ n (× n (sub1 m)))]))

(check-eq? (× 12 3) 36)


; v1 works on equal length lists

(define (tup+-v1 tup1 tup2)
  (cond
    [(and (null? tup1) (null? tup2)) '()]
    [else (cons (+ (car tup1) (car tup2))
                (tup+-v1 (cdr tup1) (cdr tup2)))]))

(check-equal? (tup+-v1 '(3 6 9 11 4) '(8 5 2 0 7)) '(11 11 11 11 11))
(check-equal? (tup+-v1 '(2 3) '(4 6)) '(6 9))


; works on lists even if not equal length

(define (tup+-v2 tup1 tup2)
  (cond
    [(and (null? tup1) (null? tup2)) '()]
    [(null? tup1) tup2]
    [(null? tup2) tup1]
    [else (cons (+ (car tup1) (car tup2))
                (tup+-v2 (cdr tup1) (cdr tup2)))]))

; simplified

(define (tup+ tup1 tup2)
  (cond
    [(null? tup1) tup2]
    [(null? tup2) tup1]
    [else (cons (+ (car tup1) (car tup2))
                (tup+ (cdr tup1) (cdr tup2)))]))

(check-equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))
(check-equal? (tup+ '(3 7 8 1) '(4 6)) '(7 13 8 1))


; version that doesn't handle equal numbers

(define (•>-v1 n m)
  (cond
    [(zero? m) #t]
    [(zero? n) #f]
    [else (•>-v1 (sub1 n) (sub1 m))]))

(check-false (•>-v1 12 133))
(check-true (•>-v1 120 11))
; provides wrong answer
;(check-false (•> 3 3)


; version that does

(define (•> n m)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (•> (sub1 n) (sub1 m))]))

(check-false (•> 12 133))
(check-true (•> 120 11))
(check-false (•> 3 3))


(define (•< n m)
  (cond
    [(zero? m) #f]
    [(zero? n) #t]
    [else (•< (sub1 n) (sub1 m))]))

(check-true (•< 4 6))
(check-false (•< 8 3))
(check-false (•< 6 6))


(define (•=-v1 n m)
  (cond
    [(zero? m) (zero? n)]
    [(zero? n) #f]
    [else (•= (sub1 n) (sub1 m))]))

; version using •< and •>

(define (•= n m)
  (cond
    [(•< n m) #f]
    [(•> n m) #f]
    [else #t]))

(check-true (•= 3 3))
(check-false (•= 3 4))


(define (↑ n m)
  (cond
    [(zero? m) 1]
    [else (* n (↑ n (sub1 m)))]))

(check-eq? (↑ 1 1) 1)
(check-eq? (↑ 2 3) 8)
(check-eq? (↑ 5 3) 125)


(define (÷ n m)
  (cond
    [(< n m) 0]
    [else (add1 (÷ (- n m) m))]))

(check-eq? (÷ 15 4) 3)


(define (•length lat)
  (cond
    [(null? lat) 0]
    [else (add1 (•length (cdr lat)))]))

(check-eq? (•length '(hotdogs with mustard sauerkraut and pickles)) 6)
(check-eq? (•length '(ham and cheese on rye)) 5)


(define (•pick n lat)
  (cond
    [(zero? (sub1 n)) (car lat)]
    [else (•pick (sub1 n) (cdr lat))]))

(check-eq? (•pick 4 '(las spag ravioli macaroni meatball)) 'macaroni)
; should get "no answer" [failure]
(check-exn exn:fail? (λ () (•pick 0 '(a))) '())


(define (rempick n lat)
  (cond
    [(zero? (sub1 n)) (cdr lat)]
    [else (cons (car lat)
                (rempick (sub1 n) (cdr lat)))]))

(check-equal? (rempick 3 '(hotdogs with hot mustard))
              '(hotdogs with mustard))


(define (no-nums lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(number? (car lat)) (no-nums (cdr lat))]
       [else (cons (car lat)
                   (no-nums (cdr lat)))])]))

(check-equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))



(define (all-nums lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(number? (car lat)) (cons (car lat)
                                  (all-nums (cdr lat)))]
       [else (all-nums (cdr lat))])]))

(check-equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))


; eqan? isn't really needed in racket, since eq? works for numbers,
; too, but ...

(define (eqan? a1 a2)
  (cond
    [(and (number? a1) (number? a2)) (= a1 a2)]
    [(or (number? a1) (number? a2)) #f]
    [else (eq? a1 a2)]))

(check-true (eqan? 1 1))
(check-false (eqan? 1 2))
(check-true (eqan? 'a 'a))
(check-false (eqan? 'a 'b))


(define (occur a lat)
  (cond
    [(null? lat) 0]
    [else
     (cond
       [(eqan? (car lat) a) (add1 (occur a (cdr lat)))]
       [else (occur a (cdr lat))])]))

(check-eq? (occur 'a '(a b c d a)) 2)


(define (one?-v1 n)
  (cond
    [(zero? n) #f]
    [else (zero? (sub1 n))]))

(define (one? n)
  (= n 1))

(check-true (one? 1))
(check-false (one? 2))


; version using one?

(define (rempick-v2 n lat)
  (cond
    [(one? n) (cdr lat)]
    [else (cons (car lat)
                (rempick-v2 (sub1 n) (cdr lat)))]))

(check-equal? (rempick-v2 3 '(lemon meringue salty pie))
              '(lemon meringue pie))


(provide eqan?)