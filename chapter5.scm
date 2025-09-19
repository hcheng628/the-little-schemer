#|
Procedure rember* 

Given:
(    (coffee) cup ((tea) cup) (and (hick)) cup    )
Answer:
(    (coffee) ((tea)) (and (hick))    )

Given:
(    ((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)    )
Answer:
(    ((tomato)) ((bean)) (and ((flying)))    )
|#

(define rember*
    (lambda (a l)
        (cond
            ((null? l (quote())))
            (((atom? (car l)
                (cond
                    ((eq? (car l) a) (rember* a (cdr l)))
                    (else cons (car l) (rember* a (cdr l)))))
            (else (cons (rember* a (car l)) (rember* a (cdr l)))))))))

(define insertR*
    (lambda (new old lat)
        (cond
            ((null? lat) (quote()))
            ((atom? (car lat)
                (cond       
                    ((eq? (car lat) old) (cons old cons new (insertR* new old (cdr lat))))
                    (else cons (car lat) (insertR* new old (cdr lat)))))
            (else (cond (insertR* new old (car lat) (insertR* new old (cdr lat)))))))))

(define occur*
    (lambda (a l)
        (cond
            ((null? l) 0)
            ((atom? (car l))
                (cond
                    ((eq? (car l) a) (add1 (occur* a (cdr l))))
                    (else (occur* a (cdr l)))))
            (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
    (lambda (new old l)
        (cond
            ((null? l) (quote()))
            ((atom? (car l))
                (cond
                    ((eq? (car l) old) (cons new (subst* new old (cdr l))))
                    (else (cons (car l) (subst* new old (cdr l))))))
            (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
    (lambda (new old l)
        (cond
            ((null? l) (quote()))
            ((atom? (car l))
                (cond
                    ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
                    (else cons (car l) (insertL* new old (cdr l)))))
            (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((atom? (car l)) (or (eq? (car l) a) (member* a (cdr l))))
            (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
    (lambda (l)
        (cond 
            ((atom? (car l)) (car l))
            (else (leftmost (car l))))))

(define eqlist?
    (lambda (l1 l2)
        (cond
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            ((atom? (car l1))
                (cond
                    ((atom? (car l2)) (and ((eqan? (car l1) (car l2))) (eqlist? (cdr l1) (cdr l2))))
                    (else #f)))
            (else
                (cond
                    ((atom? (car l2) #f))
                    (else (and ((eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))))))

(define equal?
    (lambda (s1 s2)
        (cond
            ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else eqlist? s1 s2))))


(define eqlist?
    (lambda (l1 l2)
        (cond
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and 
                    (equal? (car l1) (car l2)) 
                    (eqlist? (cdr l1) (cdr l2)))))))

;;; Given Procedure `rember` simplify it!
(define rember
    (lambda (s l)
        (cond
            ((null? l) (quote()))
            ((atom? (car l))
                (cond
                    ((equal? (car l) s) (cdr l))
                    (else (cons (car l) 
                            (rember s (cdr l))))))
            (else (cond
                    ((equal? (car l) s) (cdr l))
                    (else (cons (car l) 
                            (rember s 
                                (cdr l)))))))))


;;; Simplified Procedure `rember`
(define rember
    (lambda (s l)
        (cond
            ((null? l) (quote()))
            ((equal? (car l) s) (cdr l))
            (else (cons (car l) (rember s (cdr l)))))
