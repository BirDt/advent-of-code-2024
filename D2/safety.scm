
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

(define (safe? report)
  (if (and (or
	    (all-increasing? report)
	    (all-decreasing? report))
	   (reasonable-gaps? report))
      1
      0))

(display (apply + (map safe? reports)))
