(letrec* ((a 12) 
          (b (let* ((t 21)) -4)))
    (+ a 
       (begin 
          (set! a -7) 
          a)))

          