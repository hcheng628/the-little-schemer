;;; Try to write set? using member?
(define set?
    (lambda (lat)
        (cond
            ((null? lat) #t)
            ((member? (car lat) (cdr lat)) #f)
            (else (set? (cdr lat))))))

;;; Try to write makeset using member?
(define makeset
    (lamdba (lat)
        (cond
            ((null? lat) (quote()))
            ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
            (else (cons (car lat) (makeset (cdr lat))))

;;; Try to write makeset using multirember
(define makeset
    (lamdba (lat)
        (cond
            ((null? lat) (quote()))
            (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

;;; Try to  write subset?
(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            ((member? (car set1) set2) (subset? (cdr set1) set2))
            (else #f))))


(define subset?
    (lambda (set1 set2)
        (cond
            ((null? set1) #t)
            (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

;;; set1: (6 large chickens with wings) set2: (6 chickens with large wings)
(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))


;;; Try to write intersect?
(define intersect?
    (lambda (set1 set2)
        (cond
            ((null? set1) #f)
            (else (or ((member? (car set1) set2) #t) (intersect? (cdr set1) set2))))))

;;; Try to write intersect
(define intersect
    (lambda (set1 set2)
        (cond
        ((null? set1) (quote()))
        ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
        (else intersect (cdr set1) set2))))

;;; Try to write union
(define union
    (lambda (set1 set2)
        (cond
            ((null? set1) set2)
            ((member? (car set1) set2) (union (cdr set1) set2))
            (else (union (cdr set1) set2)))))

;;; Try to write intersectall
(define intersectall
    (lambda (l-set)
        (cond
            ((null? l-set) (quote()))
            ((null? (cdr l-set)) (car l-set))
            (else (intersect (car l-set) (intersectall (cdr l-set)))))))


;;; Try to write a-pair?
(define a-pair?
    (lambda (x)
        (cond
            ((or (null? x) (atom? x)) #f)
            ((null? (cdr x)) #f)
            ((null? (cdr (cdr x))) #t)
            (else #f))))

(define first
    (lambda (p)
        (cond (else (car p)))))

(define second
    (lambda (p)
        (car (cdr p))))

(define build
    (lambda (s1 s2)
        (cons s1 (cons s2 (quote())))))

;;; Try to write a-pair?
(define third
    (lambda (l)
        (car (cdr (cdr l)))))

;;; Try to write fun?
(define fun?
    (lambda (rel)
        set? (firsts rel)))

;;; Try to write revrel
(define revrel
    (lambda (rel)
        (cond
            ((null? rel) (quote()))
            ((cons (build (second (car rel)) (first (car rel)) revrel (cdr rel))))

;;; Given revpair
(define revpair
    (lambda (pair)
        (build (second pair) (first pair))))

;;; Try to write revrel again using revpair
(define revrel
    (lambda (rel)
        (cond
            ((null? rel) (quote()))
            ((cons (revpair (car rel)) revrel (cdr rel))))))


;;; Try to write fullfun?
(define fullfun
    (lambda (fun)
        (set? (firsts (revrel fun)))))

;;; Try to write seconds
(define seconds
    (lambda (l)
        (cond
            ((null? l) (quote()))
            ((cons (second l) seconds (cdr l)))

;;; Try to write fullfun? with seconds
(define fullfun
    (lambda (fun)
        (set? (seconds fun))))

;;; Try to write one-to-one (fullfun)
(define one-to-one
    (lambda (fun)
        (fun? (revrel fun))))
