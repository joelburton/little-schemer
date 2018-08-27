#lang racket/base

(require rackunit)
(require "1.rkt")
(require "6.rkt") ; operator, 1st-sub-exp, 2nd-sub-exp

(define (rember-f-v1 test? a l)
  (cond
    [(null? l) '()]
    [(test? (car l) a) (cdr l)]
    [else (cons (car l)
                (rember-f-v1 test? a (cdr l)))]))

(check-equal? (rember-f-v1 = 5 '(6 2 5 3)) '(6 2 3))
(check-equal? (rember-f-v1 eq? 'jelly '(jelly beans are good))
              '(beans are good))
(check-equal? (rember-f-v1 equal?
                           '(pop corn)
                           '(lemonade (pop corn) and (cake)))
              '(lemonade and (cake)))


(define (eq?-c a)
  (λ (x) (eq? x a)))

(define eq?-salad (eq?-c 'salad))
(check-true (eq?-salad 'salad))
(check-false (eq?-salad 'tuna))
(check-true ((eq?-c 'salad) 'salad))

; version that makes functions

(define (rember-f test?)
  (λ (a l)
    (cond
      [(null? l) '()]
      [(test? (car l) a) (cdr l)]
      [else (cons (car l)
                  ((rember-f test?) a (cdr l)))])))

(check-equal? ((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
              '(shrimp salad and salad))
(check-equal? ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
              '(equal? eqan? eqlist? eqpair?))


(define (insertL-f test?)
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(test? (car l) old) (cons new (cons old (cdr l)))]
      [else (cons (car l)
                  ((insertL-f test?) new old (cdr l)))])))

(check-equal? ((insertL-f eq?) 'a 1 '(1 2 1 2)) '(a 1 2 1 2))

(define (insertR-f test?)
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(test? (car l) old) (cons old (cons new (cdr l)))]
      [else (cons (car l)
                  ((insertR-f test?) new old (cdr l)))])))

(check-equal? ((insertR-f eq?) 'a 1 '(1 2 1 2)) '(1 a 2 1 2))



(define (seqL new old l) (cons new (cons old l)))
(define (seqR new old l) (cons old (cons new l)))

(define (insert-g seq)
  (λ (new old l)
    (cond
      [(null? l) '()]
      [(eq? (car l) old) (seq new old (cdr l))]
      [else (cons (car l)
                  ((insert-g seq) new old (cdr l)))])))

(check-equal? ((insert-g seqL) 'a 1 '(1 2 1 2)) '(a 1 2 1 2))
(check-equal? ((insert-g seqR) 'a 1 '(1 2 1 2)) '(1 a 2 1 2))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))
(check-equal? (insertL 'a 1 '(1 2 1 2)) '(a 1 2 1 2))

(define insertL-inline (insert-g (λ (new old l) (cons new (cons old l)))))
(check-equal? (insertL-inline 'a 1 '(1 2 1 2)) '(a 1 2 1 2))


(define (seqS new old l) (cons new l))
(define subst (insert-g seqS))
(check-equal? (subst 'a 1 '(1 2 1 2)) '(a 2 1 2))


(define (yyy a l) ((insert-g seqrem) #f a l))
(define (seqrem new old l) l)
(check-equal? (yyy 'sausage '(pizza with sausage and bacon))
              '(pizza with and bacon))


(define (atom-to-function x)
  (cond
    [(eq? x '+) +]
    [(eq? x '×) *]
    [else expt]))

(check-equal? (atom-to-function (operator '(+ 5 3))) +)

(define (value nexp)
  (cond
    [(atom? nexp) nexp]
    [else
     ((atom-to-function (operator nexp))
      (value (1st-sub-exp nexp))
      (value (2nd-sub-exp nexp)))]))

(check-eq? (value '(+ 1 (× 2 3))) 7)



(define (multirember-f test?)
  (λ (a lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat) a)
       ((multirember-f test?) a (cdr lat))]
      [else (cons (car lat)
                  ((multirember-f test?) a (cdr lat)))])))

(check-equal? ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))

(define multirember-eq (multirember-f eq?))


(define eq?-tuna (eq?-c 'tuna))
(check-true (eq?-tuna 'tuna))


(define (multiremberT test? lat)
  (cond
    [(null? lat) '()]
    [(test? (car lat)) (multiremberT test? (cdr lat))]
    [else (cons (car lat)
                (multiremberT test? (cdr lat)))]))

(check-equal? (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
              '(shrimp salad salad and))



; collectors!

(define (multirember&co a lat col)
  (cond
    [(null? lat) (col '() '())]
    [(eq? (car lat) a)
     (multirember&co a
                     (cdr lat)
                     (λ (newlat seen)
                       (col newlat
                            (cons (car lat) seen))))]
    [else
     (multirember&co a
                     (cdr lat)
                     (λ (newlat seen)
                       (col (cons (car lat) newlat)
                            seen)))]))


(define (a-friend x y) (null? y))
(check-true (multirember&co 'tuna '() a-friend))
(check-false (multirember&co 'tuna '(tuna) a-friend))
(check-false (multirember&co 'tuna '(tuna and) a-friend))

; find number of non-tuna things
(check-eq? (multirember&co 'tuna
                           '(strawb tuna and swordfish)
                           (λ (x y) (length x)))
           3)

; effectively, this becomes "did multirember&co remove nothing?
(check-false (multirember&co 'tuna '(strawb tuna and swordfish) a-friend))
(check-true (multirember&co 'tofu '(strawb tuna and swordfish) a-friend))

;demo of multirember&co -- which is basically "partition"
(let ([show (λ (newlat seen) (cons newlat seen))])
  (check-equal? (multirember&co 'a '(1 2 3 4 5) show) '((1 2 3 4 5) . ())) 
  (check-equal? (multirember&co '2 '(1 2 3 4 5) show) '((1 3 4 5) . (2)))
  (check-equal? (multirember&co '2 '(1 2 3 2 5) show) '((1 3 5) . (2 2))))



(define (multiinsertLR new oldL oldR lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) oldL)
     (cons new
           (cons oldL
                 (multiinsertLR new oldL oldR (cdr lat))))]
    [(eq? (car lat) oldR)
     (cons oldR
           (cons new
                 (multiinsertLR new oldL oldR (cdr lat))))]
    [else
     (cons (car lat)
           (multiinsertLR new oldL oldR (cdr lat)))]))


(check-equal? (multiinsertLR 'A 1 2 '(1 2 3 1 2)) '(A 1 2 A 3 A 1 2 A))


; same, but probably easier-to-read, by factoring out processed-rest

(define (multiinsertLR-v2 new oldL oldR lat)
  (cond
    [(null? lat) '()]
    [else
     (let ([processed-rest (multiinsertLR-v2 new oldL oldR (cdr lat))])
       (cond
         [(eq? (car lat) oldL) (cons new (cons oldL processed-rest))]
         [(eq? (car lat) oldR) (cons oldR (cons new processed-rest))]
         [else                 (cons (car lat) processed-rest)]))]))


(define (multiinsertLR&co new oldL oldR lat col)
  (cond
    [(null? lat) (col '() 0 0)]
    [(eq? (car lat) oldL)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (λ (newlat L R)
                         (col
                          (cons new (cons oldL newlat))
                          (add1 L)
                          R)))]
    [(eq? (car lat) oldR)
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (λ (newlat L R)
                         (col
                          (cons oldR (cons new newlat))
                          L
                          (add1 R))))]
    [else
     (multiinsertLR&co new oldL oldR (cdr lat)
                       (λ (newlat L R)
                         (col
                          (cons (car lat) newlat)
                          L
                          R)))]))

(let ([show (λ (newlat L R) (format "~a ~a ~a" newlat L R))])
  (check-equal? (multiinsertLR&co 'a 1 2 '(1 2 1) show) "(a 1 2 a a 1) 2 1")
  (check-equal? (multiinsertLR&co 'a 1 2 '(4 4) show) "(4 4) 0 0"))


(define (•even? n)
  ; the definition they give for even doesn't work in racket, since / is FP division.
  ; it could be:
  ;    (= (* (quotient n 2) 2) n)
  ; but much easier is this:
  (zero? (modulo n 2)))

(check-true (•even? 4))
(check-false (•even? 7))

(define (evens-only* l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(•even? (car l)) (cons (car l) (evens-only* (cdr l)))]
       [else             (evens-only* (cdr l))])]
    [else (cons
           (evens-only* (car l))
           (evens-only* (cdr l)))]))

(check-equal? (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) '((2 8) 10 (() 6) 2))


(define (evens-only*&co l col)
  (cond
    [(null? l) (col '() 1 0)]
    [(atom? (car l))
     (cond
       [(•even? (car l)) (evens-only*&co (cdr l)
                                         (λ (newl p s)
                                           (col (cons (car l) newl)
                                                (* (car l) p)
                                                s)))]
       [else (evens-only*&co (cdr l)
                             (λ (newl p s)
                               (col newl
                                    p
                                    (+ (car l) s))))])]
    [else (evens-only*&co (car l)
                          (λ (al ap as)
                            (evens-only*&co (cdr l)
                                            (λ (dl dp ds)
                                              (col (cons al dl)
                                                   (* ap dp)
                                                   (+ as ds))))))]))

(define (the-last-friend newl product sum)
  (cons sum (cons product newl)))

(check-equal? (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
              '(38 1920 (2 8) 10 (() 6) 2))
        
            
           
            
