(letrec* ((s 11) 
          (j 5)) 
    (begin 
        (set! s 20) 
        (set! s j) 
        s))