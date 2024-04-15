(let ((x 2)
      (y 6)
      (z 7))
  (if (< z 6)       
    8    
    (begin (set! x (* y -2))))  
  x)
