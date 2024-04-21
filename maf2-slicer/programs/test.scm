(define a 6)
(define b 7)
(define x 8)
(if (> a b)
    (set! x a)
    (set! b 0))
x
