(define lookup-in-entry
    (lambda (name entry entry-f)
        (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
    (lambda (name names values entry-f)
        (cond
            ((null? names) (entry-f name))
            ((eq? (car names) name) (car values))
            (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)

;;; cons an atom to quote() will be (atom)
;;; cons an (???) to quote() will be ((???))
;;; rep-car: car rep-quote: quote rep-a: a rep-b: b rep-c: c
;;; (car (quote (a b c)))
(cons rep-car
    (cons
        (cons rep-quote
            (cons
                (cons rep-a
                    (cons rep-b
                        (cons rep-c
                            (quote())
                        )
                    )
                )
                (quote())
            )
        )
        (quote())
    )
)

;;; The value is ((from nothing comes something))
(
    (lambda (nothing)
        (cons nothing (quote()))
    )
    (quote (from nothing comes something))
)

;;; The value is something
(
    (lambda (nothing)
        (cond
            (nothing (quote something))
            (else (quote nothing)))
    ) #t
)

;;; How many types do you think there are
;;; 1. *const
;;; 2. *quote
;;; 3. *identifier
;;; 4. *lambda
;;; 5. *cond
;;; 6. *application

(define expression-to-action
    (lambda (e)
        (cond
            ((atom? e) (atom-to-action) e)
            (else (list-to-action e)))))

(define atom-to-action
    (lambda (e)
        ((number? e) *const)
        ((or (eq? e #t) (eq? e #f)) *const)
        ((eq? (quote cons) e) *const)
        ((eq? (quote car) e) *const)
        ((eq? (quote cdr) e) *const)
        ((eq? (quote null?) e) *const)
        ((eq? (quote eq?) e) *const)
        ((eq? (quote atom?) e) *const)
        ((eq? (quote zero?) e) *const)
        ((eq? (quote numbers?) e) *const)
        ((eq? (quote add1) e) *const)
        ((eq? (quote sub1) e) *const)
        (else *identifier)))

(define list-to-action
    (lambda (e)
        ((atom? (car e))
            ((eq? (quote quote) (car e)) *quote)
            ((eq? (quote lambda) (car e)) *lambda)
            ((eq? (quote cond) (car e)) *cond)
            (else *application))
        (else *application)))

(define value
    (lambda (e)
        (meaning e (quote ()))))
        
(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))

;;; lambda lambda lambda
;;; when lookup-in-entry cannot find anything, it will then rec on lookup-in-table
(define lookup-in-table
    (lambda (name table table-f)
        (cond
            ((null? table) (table-f name))
            (else (lookup-in-entry name 
                (car table) 
                (lambda (name)
                    (lookup-in-table name
                        (cdr table) table-f)))))))

;;; ((from nothing comes something))
(
    (lambda (nothing)
        (cons nothing (quote()))
    )
    (quote (from nothing comes something))
)

(
    (lambda (nothing)
        (cond
            (nothing (quote something))
            (else (quote nothing))
        )
    ) #t
)


(define evcon
    (lambda (lines table)
        (cond
            (
                (else? (question-of (car lines))) 
                    (meaning (answer-of (car lines)) table)
            )
            (
                (meaning (question-of (car lines)) table) 
                    (meaning (answer-of (car lines)) table)
            )
            (else (
                evcon (cdr lines) table)
            )
        )))

(define else?
    (lambda (x)
        (and (atom? x) (eq? x (quote else)))
        (else #f)))

(define question-of
    (lambda (line)
        (first line)))
(define question-of first)

(define answer-of
    (lambda (line)
        (second line)))

(cond
    (coffee klatsch)
    (else party)
)

(
    (
        (coffee) 
        (#t)
    )
    (
        (klatsch party) 
        (5 (6))
    )
)

;;; An application is a list of expressions
;;; These expressions car position contains an expression
;;; This expression value is a function!

;;; An application must always determine the meaning of all its arguments.
;;; In other words, before we can apply a function,
;;; we have to get the meaning of all of its arguments.

(define evlis
    (lambda (args table)
        (cond
            ((null? args) (quote()))
            (else (cons (meaning (car args) table) 
                        (evlis (cdr args) table))))))

;;; What else do we need before we can determine the meaning of an application?
;;; We need to find out what its 'function-of' means

;;; Then we 1. apply the 
;;;     2. meaning of the function to the 
;;;     3. meaning of the arguments.

(define *application
    (lambda (e table)
        (apply
            (meaning (function-of e) table) (evlis (arguments-of e) table)
        )))