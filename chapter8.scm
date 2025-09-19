;;; Try to write rember-f

(define rember-f
    (lambda (test? a l)
        (cond
            ((null? l) (quote()))
            ((test? (car l) a) (cdr l))
            (else (cons (car l) (rember-f test? a (cdr l)))))))


; In computer science, Currying is the process of transforming a function that takes multiple arguments 
; into a sequence of functions, each taking a single argument. 
; Instead of a single function call for all arguments, Currying results in a series of functions where 
; each function call accepts one argument and returns a new function to accept the next, 
; until all original arguments are provided. 
; This technique, named after mathematician Haskell Curry, is fundamental to functional programming and 
; offers advantages like improved code reusability and more flexible data processing through partial application.
; Currying is a concept that breaks down functions into simpler, single-argument bits, 
; which can be pieced together to form robust pipelines, streamlining your data processing tasks. 
; It's where the magic of functional programming truly shines.

; Moses Schonfinkel (摩西芬利)是德国数学家, 1924年发表了关于组合子的论文, 提出了函数演算Function Calculus和组合子技术
; 被视为 lambda演算的先驱.

;;; Try to rewrite rember-f as a function of one argument test? that returns an argument 
;;; like rember with eq? replaced by test?

(define rember-f
    (lambda (test?)
        (lambda (a l)
            (cond
                ((null? l) (quote()))
                ((test? (car l) a) (cdr l))
                (else (cons (car l) ((rember-f test?) a (cdr l))))))))

;;; where a: "moka" and l: "pot" and it returns false.
((rember-f eq?) a l)

;;; Try to rewrite insertL as insertL-f
(define insertL-f
    (lambda (test?)
        (lambda (new old l)
            (cond
                ((null? l (quote()))
                ((test? (car l) old) (cons new l))  ; (cons new (cons old (cdr l)))
                (else (cons (car l) ((insertL-f test?) new old (cdr l)))))))))

;;; Try to rewrite insertR as insertR-f
(define insertR-f
    (lambda (test?)
        (lambda (new old l)
            (cond
                ((null? l (quote()))
                ((test? (car l) old) (cons old (cons new (cdr l))))
                (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))))

;;; Try to rewrite insert-g
(define insert-g
    (lambda (left?)
        (lambda (test?)
            (lambda (new old l)
                (cond
                    ((null? l (quote()))
                    ((test? (car l) old) 
                        (cond
                            (left? (cons new l)) 
                            (else (cons old (cons new (cdr l)))))
                    (else (cons (car l) (((insert-g left?) test?) new old (cdr l)))))))))))

(define seqL
    (lambda (new old l)
        (cons new (cons old l))))

(define seqR
    (lambda (new old l)
        (cons old (cons new l))))

;;; Try to write the funtion insert-g of one argument seq which can be either above functions
(define insert-g
    (lambda (seq)
        (lambda (test?)
            (lambda (new old l)
                (cond
                    ((null? l (quote()))
                    ((test? (car l) old) (seq new old (cdr l)))
                    (else (cons (car l) (((insert-g seq) test?) new old (cdr l))))))))))

;;; Now define insertL with insert-g
(define insertL ((insert-g seqL) eq?))

;;; Now define insertR with insert-g
(define insertR ((insert-g seqR) eq?))

(define seqS
    (lambda (new old l)
        (cons new l)))

;;; Now define subst using insert-g
(define subst ((insert-g seqS) eq?))

(define subst (
    (insert-g 
        (lambda (new old l)
            (cons new l))
    ) eq?))

(define yyy
    (lambda (a l)
        ((insert-g seqrem) #f a l)

(define seqrem
    (lambda (new old l)
        l))

;;; yyy is rember

; !!!What you have just seen is the power of abstraction!!!

;;; Try to write the function atom-to-function
(define atom-to-function
    (lambda (x)
        (cond
            ((eq? x (quote +)) +)
            ((eq? x (quote x)) x)
            (else ↑)))
;;; Try to rewrite value with only 2 cond-lines using atom-to-function
(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            (else ((atom-to-function (operator nexp)) 
                (value (1st-sub-exp nexp)) 
                (value (2nd-sub-exp nexp)))))))

;;; Given multirember Try to write multirember-f
(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) (quote()))
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
    (lambda (test?)
        (lambda (a lat)
            (cond
                ((null? lat) (quote()))
                ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
                (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

;;; Try to write multirember-eq? using multirember-f
(define multirember-eq?
    (multirember-f eq?))

;;; Try to write multiremberT
(define multiremberT
    (lambda (test? lat)
        (cond
            ((null? lat) (quote()))
            ((test? (car lat)) (multiremberT test? (cdr lat)))
            (else (cons (car lat) (multiremberT test? (cdr lat)))))))

;;; Given multirember&co - Now that looks really complicated! 
(define multirember&co
    (lambda (a lat col)
        (cond
            ((null? lat) (col (quote()) (quote())))
            ((eq? (car lat) a) (multirember&co a (cdr lat) 
                (lambda (newlat seen) 
                    (col newlat (cons (car lat) seen)))))       ; (newlat) (curr, seen)
            (else (multirember&co a (cdr lat) 
                (lambda (newlat seen) 
                    (col (cons (car lat) newlat) seen)))))))    ; (curr, newlat) (seen)

(define a-friend
    (lambda (x y) 
        (null? y)))

(define new-friend
    (lambda (newlat seen)
        (col newlat (cons (car lat) seen))))    ; (newlat) (curr, seen) and col may be a-friend

(define latest-friend
    (lambda (newlat seen)
        (a-friend (cons (quote and) newlat) seen))) ; (and newlat) (seen)

(define last-friend
    (lambda (x y)
        (length x)))

(define multiinsertLR
    (lambda (new oldL oldR lat)
        (cond
            ((null? lat) (quote()))
            ((eq? (car lat) oldL) 
                (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
            ((eq? (car lat) oldR)
                (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
            (else 
                (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
    (lambda (new oldL oldR lat col)
        (cond
            ((null? lat) col ((quote()) 0 0))
            ((eq? (car lat) oldL)
                (multiinsertLR&co new oldL oldR (cdr lat)
                    (lambda (newlat L R)
                        (col (cons new (cons oldL newlat)) (add1 L) R)
                        )))
            ((eq? (car lat) oldR)
                (multiinsertLR&co new oldL oldR (cdr lat)
                    (lambda (newlat L R)
                        (col (cons oldR (cons new newlat)) L (add1 R))
                        )))
            (else
                (multiinsertLR&co new oldL oldR (cdr lat)
                    (lambda (newlat L R)
                        (col (cons (car lat) newlat) L R)
                        )))
        )))

(define evens-only*
    (lambda (l)
        (cond
            ((null? l) (quote()))
            ((atom? (car l))
                (cond
                    ((and (number? (car l)) (even? (car l))) (cons (car l) evens-only* (cdr l)))
                    (else evens-only* (cdr l)))
            (else cons (evens-only* (car l)) (evens-only* (cdr l)))))))

;;; evens-only*&co
;;; It builds a nested list of even numbers by removing the odd ones from its argument
;;; and simulatenously multiplies the even numbers and sums up the odd numbers
;;; that occur in its argument
(define evens-only*&co
    (lambda (l col)
        (cond
            ((null? l) 0)
            ((atom? (car l))
                (cond
                    ((even? (car l)) evens-only*&co (cdr l) (lambda (a b) (x a b)))
                    (else evens-only*&co (cdr l) (lambda (a b) (+ a b))))
            (else 
                (col (evens-only*&co (car l) col) (evens-only*&co (cdr l) col))
            ))))

(define evens-only*&co
    (lambda (l col)
        (cond
            ((null? l) (col (quote()) 1 0))
            ((atom? (car l)
                (cond
                    ((even? (car l)) evens-only*&co (cdr l) 
                        (lambda (newl p s) (
                            (col (cons (car l) newl) (x (car l) p) s))))
                    (else evens-only*&co (cdr l)
                        (lambda (newl p s) (
                            (col newl p (+ (car l) s)))))
                )))
            (else (evens-only*&co (car l) 
                    (lambda (al ap as) (
                        evens-only*&co (cdr l)
                            (lambda (dl dp ds) (
                                (col (cons al dl) (x ap dp) (+ as ds))))))
            )))))

(define the-last-friend
    (lambda (newl product sum)
        (cons sum (cons product newl))))
