(define X
    (lambda (n m)
    (cond
        ((zero? m) 0)
        (else (+ n (X n (sub1 m)))))))

(define tup+
    (lambda (tup1 tup2)
    (cond
        ((null? tup1) (quote())
        (else cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define tup+V2
    (lambda (tup1 tup2)
    (cond
        ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else cons (+ (car tup1) (car tup2)) (tup+V2 (cdr tup1) (cdr tup2)))))))

(define >
    (lambda (n m)
    (cond
        ((zero? n) #f)
        ((zero? m) #t)
        (else > (sub1 n) (sub1 m)))))

(define <
    (lambda (n m)
    (cond
        ((zero? m) #f)
        ((zero? n) #t)
        (else < (sub1 n) (sub1 m)))))
        

(define <
    (lambda (n m)
    (cond
        ((or (> n m) (< n m)) #f)
        (else #t))))

(define power
    (lambda (n m)
    (cond
    ((zero? m) 1)
    (else (x n (power n (sub1 m)))))))

(define length
    (lambda (lat)
    (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat)))))))


(define pick
    (lambda (n lat)
    (cond
    ((eq? n 1) (car lat))
    (else pick (sub1 n) (cdr lat)))))

(define rempick
    (lambda (n lat)
    (cond
    ((eq? n 1) (cdr lat))
    (else cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define no-nums
    (lambda (lat)
    (cond
    ((null? lat) (quote()))
    ((numbers? (car lat)) (no-nums (cdr lat)))
    (else cons (car lat) (no-nums (cdr lat))))))