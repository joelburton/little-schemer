#lang racket/base

(require rackunit)
(require "1.rkt")

; version of member using equal? instead of eq? from ch 2

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else
     (or (equal? (car lat) a)
         (member? a (cdr lat)))]))

; version of multirember from ch 3

(define (multirember a lat)
  (cond
    [(null? lat) '()]
    [else
     (cond
       [(eq? (car lat) a) (multirember a (cdr lat))]
       [else (cons (car lat)
                   (multirember a (cdr lat)))])]))


; set is in racket, so use •set as our name

(define (•set? lat)
  (cond
    [(null? lat) #t]
    [else
     (cond
       [(member? (car lat) (cdr lat)) #f]
       [else (•set? (cdr lat))])]))
    
(check-false (•set? '(apple peaches apple plums)))
(check-true (•set? '(apple peaches pears plums)))
(check-true (•set? '()))

; version that changes order

(define (makeset-v1 lat)
  (cond
    [(null? lat) '()]
    [(member? (car lat) (cdr lat))
     (makeset-v1 (cdr lat))]
    [else (cons (car lat)
                (makeset-v1 (cdr lat)))]))

(check-equal? (makeset-v1 '(apple peach pear peach
                                  plum apple lemon peach))
              '(pear plum apple lemon peach))


; version that preserves order w/multirember

(define (makeset-v2 lat)
  (cond
    [(null? lat) '()]
    [else
     (cons (car lat)
           (makeset-v2
            (multirember (car lat) (cdr lat))))]))

(check-equal? (makeset-v2 '(apple peach pear peach
                                  plum apple lemon peach))
              '(apple peach pear plum lemon))
(check-equal? (makeset-v2 '(apple 3 pear 4 9 apple 3 4))
              '(apple 3 pear 4 9))


(define (•subset?-v1 set1 set2)
  (cond
    [(null? set1) #t]
    [else (cond
            [(member? (car set1) set2)
             (•subset?-v1 (cdr set1) set2)]
            [else #f])]))

; simplified

(define (•subset?-v2 set1 set2)
  (cond
    [(null? set1) #t]
    [(member? (car set1) set2)
     (•subset?-v2 (cdr set1) set2)]
    [else #f]))

; even more simplified

(define (•subset? set1 set2)
  (cond
    [(null? set1) #t]
    [else (and (member? (car set1) set2)
               (•subset? (cdr set1) set2))]))

(check-true (•subset? '(5 chicken wings)
                      '(5 hamburgers
                          2 pieces fried chicken and
                          light duckling wings)))
(check-false (•subset? '(4 pounds of horseradish)
                       '(four pounds chicken and
                              5 ounces horseradish)))



(define (eqset?-v1 set1 set2)
  (cond
    [(•subset? set1 set2) (•subset? set2 set1)]
    [else #f]))

; simpler, with one cond line

(define (eqset?-v2 set1 set2)
  (cond
    [else (and (•subset? set1 set2)
               (•subset? set2 set1))]))

; simplest

(define (eqset? set1 set2)
  (and (•subset? set1 set2)
       (•subset? set2 set1)))

(check-true (eqset? '(6 large chickens with wings)
                    '(6 large chickens with wings)))
(check-false (eqset? '(6 large chickens with wings)
                     '(6 large chickens with feet wings)))
(check-false (eqset?'(6 large chickens with wings feet)
                    '(6 large chickens with wings)))


(define (intersect?-v1 set1 set2)
  (cond
    [(null? set1) #f]
    [else
     (cond
       [(member? (car set1) set2) #t]
       [else (intersect?-v1 (cdr set1) set2)])]))

; shorter

(define (intersect?-v2 set1 set2)
  (cond
    [(null? set1) #f]
    [(member? (car set1) set2) #t]
    [else (intersect?-v2 (cdr set1) set2)]))

; shorter, with or

(define (intersect? set1 set2)
  (cond
    [(null? set1) #f]
    [else (or (member? (car set1) set2)
              (intersect? (cdr set1) set2))]))

(check-true (intersect? '(stewed tomatoes and macaroni)
                        '(macaroni and cheese)))
(check-false (intersect? '(stewed tomatoes and macaroni)
                         '(lentils with bread)))


(define (intersect set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
    [else (intersect (cdr set1) set2)]))

(check-equal? (intersect '(stewed tomatoes and macaroni)
                         '(macaroni and cheese))
              '(and macaroni))
; note that this only works if set1 & set2 are actually sets,
; otherwise it gets duplicates
(check-equal? (intersect '(a b c a) '(a b)) '(a b a))  ; argh


(define (union set1 set2)
  (cond
    [(null? set1) set2]
    [(member? (car set1) set2)
     (union (cdr set1) set2)]
    [else (cons (car set1)
                (union (cdr set1) set2))]))

(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))


; difference: set1 - set2

(define (xxx set1 set2)
  (cond
    [(null? set1) '()]
    [(member? (car set1) set2)
     (xxx (cdr set1) set2)]
    [else (cons (car set1)
                (xxx (cdr set1) set2))]))

(check-equal? (xxx '(a b c 1 2 3) '(1 2 3 4)) '(a b c))


(define (intersectall l-set)
  (cond
    [(null? (cdr l-set)) (car l-set)]
    [else (intersect (car l-set)
                     (intersectall (cdr l-set)))]))


(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b))) '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples)))
              '(6 and))


(define (a-pair? x)
  (cond
    [(atom? x) #f]
    [(null? x) #f]
    [(null? (cdr x)) #f]
    [(null? (cdr (cdr x))) #t]
    [else #f]))

(check-true (a-pair? '(pear pear)))
(check-true (a-pair? '(3 7)))
(check-true (a-pair? '((2) (pair))))
(check-true (a-pair? '(full (house))))


(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (third p)
  (car (cdr (cdr p))))


; from ch 3
(define (firsts l)
  (cond
    [(null? l) '()]
    [else (cons (car (car l)) (firsts (cdr l)))]))

(define (fun? rel)
  (•set? (firsts rel)))

(check-false (fun? '((4 3) (4 2) (7 6) (6 2) (3 4))))
(check-true (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-false (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))


(define (revrel rel)
  (cond
    [(null? rel) '()]
    [else (cons (build (second (car rel)) (first (car rel)))
                (revrel (cdr rel)))]))

(check-equal? (revrel '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

; also works, but harder to read

(define (revrel-yuck rel) 
  (cond
    [(null? rel) '()]
    [else (cons (cons (car (cdr (car rel)))
                      (cons (car (car rel))
                            '()))
                (revrel-yuck (cdr rel)))]))

(check-equal? (revrel-yuck '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))

; using revpair

(define (revpair pair)
  (build (second pair) (first pair)))

(define (revrel-v3 rel)
  (cond
    [(null? rel) '()]
    [else (cons (revpair (car rel))
                (revrel-v3 (cdr rel)))]))

(check-equal? (revrel-v3 '((8 a) (pumpkin pie) (got sick)))
              '((a 8) (pie pumpkin) (sick got)))



(define (seconds l)
  (cond
    [(null? l) '()]
    [else (cons (second (car l)) (seconds (cdr l)))]))

(check-equal? (seconds '((1 10) (2 20))) '(10 20))

(define (fullfun? fun)
  (•set? (seconds fun)))

(check-false (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(check-true (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(check-false (fullfun? '((grape raisin) (plum prune) (stewed prune))))
(check-true (fullfun? '((grape raisin) (plum prune) (stewed grape))))


(define (one-to-one? fun)
  (fun? (revrel fun)))

(check-true (one-to-one? '((chocolate chip) (doughy cookie))))