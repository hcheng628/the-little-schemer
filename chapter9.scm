(define looking
    (lambda (a lat)
        (keep-looking a (pick 1 lat) lat)))

(define keep-looking
    (lambda (a sorn lat)
        ((number? pos) (keep-looking a (pick sorn lat) lat))
        (else (eq? a pos))))

(define eternity
    (lambda (x)
        (eternity x)))

;;; ((a b) c)       -> (a (b c))
;;; ((a b) (c d))   -> (a (b (c d)))
;;; Try to write shift based on the above behaviors
(define shift
    (lambda (pair)4
        (build (first (first pair)) 
            (build (second (first pair)) (second pair)))))

(define align
    (lambda (pora)
        (cond
            ((atom? pora) pora)
            ((a-pair? (first pora)) align (shift pora)))
            (else (build (first pora) (align (second pora))))))

(define length*
    (lambda (pora)
        ((atom? pora) 1)
        (else (+ (length* (car pora)) (length* (cdr pora))))))

;;; ((a b) c) = (((1 x 2) + 1) x 2) + 1 = 7
;;; (a (b c)) = (1 x 2) + (((1 x 2) + 1)) = 5
(define weight*
    (lambda (pora)
        ((atom? pora) 1)
        (else (+ (x (weight* (first pora)) 2) (weight* (second pora))))))

;;; ((a b) c)       => (c (b a))
;;; (a (b c))       => (a (b c))
;;; (a b)           => (a b)
;;; ((a b) (c d))   => 
;;; ((c d) (a b))

(define shuffle
    (lambda (pora)
        ((atom? pora) pora)
        ((a-pair? (first pora)) (shuffle (revpair pora)))
        (else (build (first pora) (shuffle (second pora))))))

(define C
    (lambda (n)
        ((one? n) 1)
        (else (cond
            ((even? n) (C (/ n 2)))
            (else (C (add1 (x 3 n))))))))

; (A 1 2) -> (A 0 (A 1 1)) -> (A 0 (A 0 2)) -> (A 0 3) = 4
; (A 4 3) -> Decayed long before we could possibly have calculated the value
(define A
    (lambda (n m)
        ((zero? n) (add1 m))
        ((zero? m) (A (sub1 n) 1))
        (else (A (sub1 n) A(n (sub1 m))))))

(define will-stop?
    (lambda (f)
        ...))

(define last-try
    (lambda (x)
        (and (will-stop? last-try) (eternity x))))

(define length
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))

;;; Determine the length of the empty list. We call this length0
(lambda (l)
    ((null? l) 0)
    (else (add1 (eternity (cdr l)))))

;;; Determine the length of a list that contains one or fewer items.
(lambda (l)
    ((null? l) 0)
    (else (add1 (length0 (cdr l)))))

;;; Almost, but (define ...) doesn't work for length0
;;; Then, we can name it as length<=1 below
(lambda (l)
    ((null? l) 0)
    (else (add1 ((lambda (l)
                    ((null? l) 0)
                    (else (add1 (eternity (cdr l))))
                ) 
                (cdr l))
)))

;;; Now, with this below, we can call this length<=2
(lambda (l)
    ((null? l) 0)
    (else (add1 ((lambda (l)
                    ((null? l) 0)
                    (else (add1 ((lambda (l)
                                    ((null? l) 0)
                                    (else (add1 (eternity (cdr l))))
                            ) (cdr l))))
            ) (cdr l))
)))

;;; A function that looks just like length but starts with (lambda (length)...).
;;; This is another version of length0
(
    (lambda (length)
        (lambda (l)
            ((null? l) 0)
            (else (add1 (length (cdr l)))))
    ) eternity
)

;;; Rewrite length<=1 in the same style
;;; (cheng)
;;; fun(fun(ete)) 
(
    (lambda (f)
        (lambda (l)
            ((null? l) 0) (else (add1 (f (cdr l))))
        )
    )
    (
        (lambda (g)
            (lambda (l)
                ((null? l) 0) (else (add1 (g (cdr l))))
            )
        ) eternity
    )
)

;;; How about length<=2
(
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
    (
        (lambda (length)
            (lambda (l)
                ((null? l) 0) (else (add1 (length (cdr l))))
            )
        )
        (
            (lambda (length)
                (lambda (l)
                    ((null? l) 0) (else (add1 (length (cdr l))))
                )
            ) eternity
        )
    )
)

;;; Lets rewrite length0
(
    (lambda (mk-length)
        (mk-length eternity)
    )
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
)
;;; Lets rewrite length<=1
(
    (lambda (mk-length)
        (mk-length (mk-length eternity))
    )
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
)

;;; Lets rewrite length<=2
(
    (lambda (mk-length)
        (mk-length (mk-length (mk-length eternity)))
    )
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
)

;;; Lets rewrite length<=4  (a b c d)
(
    (lambda (mk-length)
        (mk-length (mk-length (mk-length (mk-length eternity))))
    )
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
)

;;; Lets rewrite length0 without eternity
(
    (lambda (mk-length)
        (mk-length mk-length)
    )
    (lambda (length)
        (lambda (l)
            ((null? l) 0) (else (add1 (length (cdr l))))
        )
    )
)

;;; Lets rewrite length0 without length
(
    (lambda (mk-length)
        (mk-length mk-length)
    )
    (lambda (mk-length)
        (lambda (l)
            ((null? l) 0) (else (add1 (mk-length (cdr l))))
        )
    )
)

;;; Lets apply mk-length once
(
    (lambda (mk-length)
        (mk-length mk-length)
    )
    (lambda (mk-length)
        (lambda (l)
            ((null? l) 0) (else (add1 ((mk-length eternity) (cdr l))))
        )
    )
)

;;; l is (apples) and the answer is 1
(
    (
        (lambda (mk-length)
            (mk-length mk-length)
        )
        (lambda (mk-length)
            (lambda (l)
                ((null? l) 0) (else (add1 ((mk-length eternity) (cdr l))))
            )
        )
    ) l
)

;;; It is length, of course
(
    (lambda (mk-length)
        (mk-length mk-length)
    )
    (lambda (mk-length)
        (lambda (l)
            ((null? l) 0) (else (add1 ((mk-length mk-length) (cdr l))))
        )
    )
)

;;; The new length
(
    (lambda (mk-length)
        (mk-length mk-length)
    )
    (lambda (mk-length)
        (
            (lambda (length)
                (lambda (l)
                    ((null? l) 0) (else (add1 (length (cdr l))))
                )
            )
            (mk-length mk-length)
        )
    )
)

;;;
(
    (lambda (mk-length)
        (
            (lambda (length)
                (lambda (l)
                    ((null? l) 0) (else (add1 (length (cdr l))))
                )
            )
            (mk-length mk-length)
        )
    )
    (lambda (mk-length)
        (
            (lambda (length)
                (lambda (l)
                    ((null? l) 0) (else (add1 (length (cdr l))))
                )
            )
            (mk-length mk-length)
        )
    )
)

;;;
(
    (lambda (length)
        (lambda (l)
            (cond ((null? l) 0) (else (add1 (length (cdr l)))))
        )
    )
    (
        (lambda (mk-length)
            (
                (lambda (length)
                    (lambda (l)
                        (cond ((null? l) 0) (else (add1 (length (cdr l)))))
                    )
                ) (mk-length mk-length)
            )
        )
        (lambda (mk-length)
            (
                (lambda (length)
                    (lambda (l)
                        (cond ((null? l) 0) (else (add1 (length (cdr l)))))
                    )
                ) (mk-length mk-length)
            )
        )
    )
)

;;;

(
    (lambda (length)
        (lambda (l)
            (cond ((null? l) 0) (else (add1 (length (cdr l)))))
        )
    )
    (
        (lambda (length)
            (lambda (l)
                (cond ((null? l) 0) (else (add1 (length (cdr l)))))
            )
        )
        (
            (lambda (mk-length)
                (
                    (lambda (length)
                        (lambda (l)
                            (cond ((null? l) 0) (else (add1 (length (cdr l)))))
                        )
                    ) (mk-length mk-length)
                )
            )
            (lambda (mk-length)
                (
                    (lambda (length)
                        (lambda (l)
                            (cond ((null? l) 0) (else (add1 (length (cdr l)))))
                        )
                    ) (mk-length mk-length)
                )
            )
        )
    )
)

;;; as same above

((lambda (length)
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
((lambda (length)
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
((lambda (mk-length)
    ((lambda (length)
        (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
    (mk-length mk-length)))
(lambda (mk-length)
((lambda (length)
    (lambda (l)
        (cond
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))
    (mk-length mk-length))))))
