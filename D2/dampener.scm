
(import
  (chicken io)
  (chicken sort)
  (chicken string))

(define reports (map (lambda (x)
		       (map string->number (string-split x)))
		     (call-with-input-file "input" (lambda (port) (read-lines port)))))

(define (difference x y)
  (abs (- x y)))

(define (contains lst cond)
  (if (< 1 (apply + (map (lambda (x) (if x 1 0)) (map cond lst))))
      #t
      #f))

(define (all-increasing? report)
  (equal? report (sort report <)))

(define (all-decreasing? report)
  (equal? report (sort report >)))

(define (gaps report index accumulator)
  (if (= index (- (length report) 1))
      accumulator
      (gaps report (add1 index)
	    (append accumulator
		    (list (difference (list-ref report index)
				      (list-ref report (add1 index))))))))

(define (reasonable-gaps? report)
  (= (apply + (map (lambda (x) (if (and (< x 4) (> x 0)) 1 0)) (gaps report 0 '())))
     (- (length report) 1)))

(define (remove-helper lst to-remove i out)
  (if (= i (length lst))
      out
      (remove-helper lst to-remove (+ 1 i)
		     (if (= to-remove i)
			 out
			 (append out (list (list-ref lst i)))))))

(define (remove to-remove lst)
  (remove-helper lst to-remove 0 '()))

(define (safe? report)
  (if (and (or
	    (all-increasing? report)
	    (all-decreasing? report))
	   (reasonable-gaps? report))
      #t
      #f))

(define (dampen report index safe)
  (if (= index (length report))
      safe
      (dampen report
	      (+ 1 index)
	      (if safe
		  safe
		  (safe? (remove index report))))))

(define (dampen-safe? report)
  (dampen report 0 #f))

(define (true-safe? report)
  (if (or (safe? report)
	  (dampen-safe? report))
      1
      0))

(display (apply + (map true-safe? reports)))

