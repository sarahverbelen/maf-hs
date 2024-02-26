(define (map f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l)))))

(define (inc n) (+ n 1))

(let ((x (map inc '(1 2 3))))
    x)