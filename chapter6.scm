(define numbered?
    (lambda (aexp)
        (cond
            ((atom? aexp) (number? aexp))
            ((or (or (eq? (car (cdr aexp)) (quote +)) 
                     (eq? (car (cdr aexp)) (quote -)))
                     (eq? (car (cdr aexp)) (quote ↑))
                        and (numbered? (car aexp)) (numbered? (cdr (cdr aexp))))))))

(define numbered?
    (lambda (aexp)
        (cond
            ((atom? aexp) (number? aexp))
            ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
            ((eq? (car (cdr aexp)) (quote ↑)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    )
)

;;; (3 + (4 x 2) + (6 + 4))
;;; (3 + (4 x (1 + 2)) + (6 + 4))

(define numbered?
    (lambda (aexp)
        (cond
            ((atom? aexp) (number? aexp))
            (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define value
    (lambda (nexp)
        (cond
            ((numbered? nexp)
                (cond
                    ((atom? nexp) nexp)
                    ((eq? (car (cdr nexp)) (quote +)) (+ (car nexp) value (car (cdr (cdr nexp)))))
                    ((eq? (car (cdr nexp)) (quote x)) (x (car nexp) value (car (cdr (cdr nexp)))))
                    ((eq? (car (cdr nexp)) (quote ↑)) (↑ (car nexp) value (car (cdr (cdr nexp)))))
            )))))

;;; Given by the book: Version 1
(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) ...)
            ((eq? (car (cdr nexp)) (quote +)) ...)
            ((eq? (car (cdr nexp)) (quote -)) ...)
            (else ...))))

;;; Given by the book: Version 2
(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) (quote +)) (+ (car nexp) value (car (cdr (cdr nexp)))))
            ((eq? (car (cdr nexp)) (quote x)) (x (car nexp) value (car (cdr (cdr nexp)))))
            (else (↑ (car nexp) value (car (cdr (cdr nexp))))))))

(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (car nexp) (quote +)) (+ (value car (cdr nexp)) (value (cdr (cdr nexp)))))
            ((eq? (car nexp) (quote x)) (x (value car (cdr nexp)) (value (cdr (cdr nexp)))))
            (else (↑ (value car (cdr nexp)) (value (cdr (cdr nexp))))))))

;;; (+ (+ 4 (+ 3 3)) (+ 6 4))

(define 1st-sub-exp
    (lambda (aexp)
        (car (cdr aexp))))

(define 2nd-sub-exp
    (lambda (aexp)
        (car (cdr (cdr aexp))))))

;;; value (cdr (cdr nexp)) vs value (car value (cdr (cdr nexp)))

(define operator
    (lambda (nexp)
        (car aexp)))

(define value
    (lambda (nexp)
        (cond
            ((atom? nexp) nexp)
            ((eq? (operator nexp) (quote +)) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
            ((eq? (operator nexp) (quote x)) (x (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
            (else (↑ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp) ))))))