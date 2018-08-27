#lang racket

(require rackunit)
(require "1.rkt")

; wrong form w/o cons

(define (rember-bad a lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) a) (cdr lat)]
            [else (rember-bad a (cdr lat))])]))

; initial, longer form

(define (rember-long a lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) a) (cdr lat)]
            [else (cons (car lat) (rember-long a (cdr lat)))])]))

; improved shorter form

(define (rember a lat)
  (cond
    [(null? lat) '()]
    [(eq? (car lat) a) (cdr lat)]
    [else (cons (car lat)
                (rember a (cdr lat)))]))

(check-equal? (rember 'mint '(lamb chops and mint jelly))
              '(lamb chops and jelly))
(check-equal? (rember 'mint '(lamb chops and mint flavored mint jelly))
              '(lamb chops and flavored mint jelly))
(check-equal? (rember 'toast '(bacon lettuce and tomato))
              '(bacon lettuce and tomato))
(check-equal? (rember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea cup and hick cup))


(define (firsts l)
  (cond
    [(null? l) '()]
    [else (cons (car (car l)) (firsts (cdr l)))]))


(check-equal? (firsts '((apple peach pumpkin)
                        (plum pear cherry)
                        (grape raisin pea)
                        (bean carrot eggplant)))
              '(apple plum grape bean))
(check-equal? (firsts '((a b) (c d) (e f))) '(a c e))
(check-equal? (firsts '()) '())
(check-equal? (firsts '((five plums)
                        (four)
                        (eleven green oranges)))
              '(five four eleven))
(check-equal? (firsts '(((five plums) four)
                        (eleven green oranges)
                        ((no) more)))
              '((five plums) eleven (no)))


(define (insertR new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
            [else (cons (car lat) (insertR new old (cdr lat)))])]))

(check-equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with fudge topping for dessert))
(check-equal? (insertR 'jalapeno 'and '(tacos tamales and salsa))
              '(tacos tamales and jalapeno salsa))
(check-equal? (insertR 'e 'd '(a b c d f g d h)) '(a b c d e f g d h))


; first version

(define (insertL-v1 new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) old) (cons new (cons old (cdr lat)))]
            [else (cons (car lat) (insertL-v1 new old (cdr lat)))])]))

; better version

(define (insertL new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) old) (cons new lat)]
            [else (cons (car lat) (insertL new old (cdr lat)))])]))

(check-equal? (insertL 'A 'a '(1 2 3 a 4 a)) '(1 2 3 A a 4 a))



(define (subst new old lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) old) (cons new (cdr lat))]
            [else (cons (car lat)
                        (subst new old (cdr lat)))])]))

(check-equal? (subst 'topping 'fudge '(ice cream with fudge for dessert))
              '(ice cream with topping for dessert))


(define (subst2-v1 new o1 o2 lat)
  (cond
    [(null? lat) '()]
    [else (cond
            [(eq? (car lat) o1) (cons new (cdr lat))]
            [(eq? (car lat) o2) (cons new (cdr lat))]
            [else (cons (car lat)
                        (subst2-v1 new o1 o2 (cdr lat)))])]))

(check-equal? (subst2-v1 'vanilla 'choc 'banana '(banana ice cream with choc topping))
              '(vanilla ice cream with choc topping))

(define (subst2 new o1 o2 lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(or (eq? (car lat) o1)
            (eq? (car lat) o2))
        (cons new (cdr lat))]
       [else (cons (car lat)
                   (subst2-v1 new o1 o2 (cdr lat)))])]))

(check-equal? (subst2 'vanilla 'choc 'banana '(banana ice cream with choc topping))
              '(vanilla ice cream with choc topping))


(define (multirember a lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) a) (multirember a (cdr lat))]
       [else (cons (car lat)
                   (multirember a (cdr lat)))])]))
                     
(check-equal? (multirember 'cup '(coffee cup tea cup and hick cup))
              '(coffee tea and hick))


(define (multiinsertR new old lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) old)
        (cons old (cons new (multiinsertR new old (cdr lat))))]
       [else (cons (car lat) (multiinsertR new old (cdr lat)))])]))

(check-equal? (multiinsertR 'A 'a '(1 2 a 3 a 4 a))
              '(1 2 a A 3 a A 4 a A))


; bad version that doesn't make progress in recursion

(define (multiinsertL-bad new old lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) old)
        (cons new (multiinsertL-bad new old lat))]
       [else (cons (car lat) (multiinsertL-bad new old (cdr lat)))])]))
              
(check-equal? (multiinsertL-bad 'A 'a '(1 2 3)) '(1 2 3))
; can't run this test, since it never stops
;(check-equal? (multiinsertL-bad 'A 'a '(1 a 3)) '(1 A a 3))

(define (multiinsertL new old lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) old)
        (cons new (cons old (multiinsertL new old (cdr lat))))]
       [else (cons (car lat) (multiinsertL new old (cdr lat)))])]))

(check-equal? (multiinsertL 'A 'a '(1 2 3)) '(1 2 3))
(check-equal? (multiinsertL 'A 'a '(1 a 3)) '(1 A a 3))

(define (multisubst new old lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) old)
        (cons new
              (multisubst new old (cdr lat)))]
       [else (cons (car lat)
                   (multisubst new old (cdr lat)))])]))

(check-equal? (multisubst 'A 'a '(a b c a)) '(A b c A))