#lang racket

(require rackunit)
(require "1.rkt")
(require "4.rkt")

(define (rember* a l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(eq? (car l) a)
        (rember* a (cdr l))]
       [else (cons (car l)
                   (rember* a (cdr l)))])]
    [else (cons (rember* a (car l))
                (rember* a (cdr l)))]))

(check-equal? (rember* 'cup '((coffee) cup ((tea) cup)
                                       (and (hick)) cup))
              '((coffee) ((tea)) (and (hick))))
(check-equal? (rember* 'sauce '(((tomato sauce))
                                ((bean) sauce)
                                (and ((flying)) sauce)))
              '(((tomato))
                ((bean))
                (and ((flying)))))


(define (insertR* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old (cdr l))))]
       [else (cons (car l)
                   (insertR* new old (cdr l)))])]
    [else (cons (insertR* new old (car l))
                (insertR* new old (cdr l)))]))
     
(check-equal? (insertR* 'roast 'chuck '((how much (wood))
                                        could
                                        ((a (wood) chuck))
                                        (((chuck)))
                                        (if (a) ((wood chuck)))
                                        could chuck wood))
              '((how much (wood))
                could
                ((a (wood) chuck roast))
                (((chuck roast)))
                (if (a) ((wood chuck roast)))
                could chuck roast wood))



(define (occur* a l)
  (cond
    [(null? l) 0]
    [(atom? (car l))
     (cond
       [(eq? (car l) a) (add1 (occur* a (cdr l)))]
       [else (occur* a (cdr l))])]
    [else (+ (occur* a (car l))
             (occur* a (cdr l)))]))

(check-eq? (occur* 'banana '((banana)
                             (split ((((banana ice)))
                                     (cream (banana))
                                     sherbet))
                             (banana)
                             (bread)
                             (banana brandy)))
           5)


(define (subst* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(eq? (car l) old)
        (cons new (subst* new old (cdr l)))]
       [else (cons (car l)
                   (subst* new old (cdr l)))])]
    [else (cons (subst* new old (car l))
                (subst* new old (cdr l)))]))
            
              
(check-equal? (subst* 'orange 'banana  '((banana)
                                         (split ((((banana ice)))
                                                 (cream (banana))
                                                 sherbet))
                                         (banana)
                                         (bread)
                                         (banana brandy)))
              '((orange)
                (split ((((orange ice)))
                        (cream (orange))
                        sherbet))
                (orange)
                (bread)
                (orange brandy)))



(define (insertL* new old l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old (cdr l))))]
       [else (cons (car l)
                   (insertL*  new old (cdr l)))])]
    [else (cons (insertL* new old (car l))
                (insertL* new old (cdr l)))]))

(check-equal? (insertL* 'pecker 'chuck '((how much (wood))
                                         could
                                         ((a (wood) chuck))
                                         (((chuck)))
                                         (if (a) ((wood chuck)))
                                         could chuck wood))
              '((how much (wood))
                could
                ((a (wood) pecker chuck))
                (((pecker chuck)))
                (if (a) ((wood pecker chuck)))
                could pecker chuck wood))


(define (member* a l)
  (cond
    [(null? l) #f]
    [(atom? (car l))
     (or (eq? (car l) a)
         (member* a (cdr l)))]
    [else (or (member* a (car l))
              (member* a (cdr l)))]))

(check-true (member* 'chips '((potato) (chips ((with) fish) (chips)))))
(check-false (member* 'a '((b c (d) e))))


(define (leftmost l)
  (cond
    [(atom? (car l)) (car l)]
    [else (leftmost (car l))]))

(check-equal? (leftmost '((potato) (chips ((with) fish) (chips))))
              'potato)
(check-equal? (leftmost '(((hot) (tuna (and))) cheese)) 'hot)
; these two will raise errors
(check-exn exn:fail? (lambda () (leftmost '(((() four)) 17 (seventeen)))))
(check-exn exn:fail? (lambda () (leftmost '())))



(define (eqlist?-v1 l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(and (null? l1) (atom? (car l2))) #f]
    [(null? l1) #f]
    [(and (atom? (car l1)) (null? l2)) #f]
    [(and (atom? (car l1))
          (atom? (car l2)))
     (and (eqan? (car l1) (car l2))
          (eqlist?-v1 (cdr l1) (cdr l2)))]
    [(atom? (car l1)) #f]
    [(null? l2) #f]
    [(atom? (car l2)) #f]
    [else
     (and (eqlist?-v1 (car l1) (car l2))
          (eqlist?-v1 (cdr l1) (cdr l2)))]))

(check-true (eqlist?-v1 '(strawberry ice cream) '(strawberry ice cream)))
(check-false (eqlist?-v1 '(strawberry ice cream) '(strawberry cream ice)))
(check-false (eqlist?-v1 '(beef ((sausage)) (and (soda)))
                         '(beef ((salami)) (and (soda)))))
(check-true (eqlist?-v1 '(beef ((sausage)) (and (soda)))
                        '(beef ((sausage)) (and (soda)))))


; simplified

(define (eqlist? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [(and (atom? (car l1)) (atom? (car l2)))
     (and (eqan? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))]
    [(or (atom? (car l1)) (atom? (car l2)) #f)]
    [else (and (eqlist? (car l1) (car l2))
               (eqlist? (cdr l1) (cdr l2)))]))

(check-true (eqlist? '(strawberry ice cream) '(strawberry ice cream)))
(check-false (eqlist? '(strawberry ice cream) '(strawberry cream ice)))
(check-false (eqlist? '(beef ((sausage)) (and (soda)))
                      '(beef ((salami)) (and (soda)))))
(check-true (eqlist? '(beef ((sausage)) (and (soda)))
                     '(beef ((sausage)) (and (soda)))))


(define (•equal?-v1 s1 s2)
  (cond
    [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
    [(atom? s1) #f]
    [(atom? s2) #f]
    [else (eqlist? s1 s2)]))

; simplified

(define (•equal? s1 s2)
  (cond
    [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
    [(or (atom? s1) (atom? s2)) #f]
    [else (eqlist? s1 s2)]))

(check-true (•equal? '(1 (2 (3 4 ))) '(1 (2 (3 4)))))
(check-false (•equal? '(1 (2 (3 4 ))) '(1 (2 (5 4)))))


; version using equal

(define (eqlist?-v3 l1 l2)
  (cond
    [(and (null? l1) (null? l2)) #t]
    [(or (null? l1) (null? l2)) #f]
    [else
     (and (•equal? (car l1) (car l2))
          (eqlist?-v3 (cdr l1) (cdr l2)))]))

(check-true (eqlist?-v3 '(strawberry ice cream) '(strawberry ice cream)))
(check-false (eqlist?-v3 '(strawberry ice cream) '(strawberry cream ice)))
(check-false (eqlist?-v3 '(beef ((sausage)) (and (soda)))
                         '(beef ((salami)) (and (soda)))))
(check-true (eqlist?-v3 '(beef ((sausage)) (and (soda)))
                        '(beef ((sausage)) (and (soda)))))


(define (rember-sexp s l)
  (cond
    [(null? l) '()]
    [(atom? (car l))
     (cond
       [(•equal? (car l) s) (cdr l)]
       [else (cons (car l)
                   (rember-sexp (cdr l)))])]
    [else (cond
            [(•equal? (car l) s) (cdr l)]
            [else (cons (car l)
                        (rember-sexp s (cdr l)))])]))

; simplified

(define (rember-sexp2 s l)
  (cond
    [(null? l) '()]
    [else
     (cond
       [(•equal? (car l) s) (cdr l)]
       [else (cons (car l)
                   (rember-sexp2 s (cdr l)))])]))

; simplified more

(define (rember-sexp3 s l)
  (cond
    [(null? l) '()]
    [(•equal? (car l) s) (cdr l)]
    [else (cons (car l)
                (rember-sexp3 s (cdr l)))]))

(check-equal? (rember-sexp3 'a '(a)) '())
(check-equal? (rember-sexp3 'a '(a (b (c (a d))))) '((b (c (a d)))))