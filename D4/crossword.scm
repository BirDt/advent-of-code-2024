(import
  (chicken io)
  (chicken string))

(define cross (map (lambda (x) (string-chop x 1))
		   (call-with-input-file "input" (lambda (port) (read-lines port)))))

;; y = vertical index
;; x = horiz index

(define max-y (length cross))

(define max-x (length (car cross)))

(define (get-y y)
  (list-ref cross y))

(define (get-x x y)
  (if (or (= y max-y) (= x max-x))
      "."
      (list-ref (get-y y) x)))

(define (left x y)
  (if (< x 3)
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (- x 1) y))
	   (equal? "A" (get-x (- x 2) y))
	   (equal? "S" (get-x (- x 3) y)))))

(define (right x y)
  (if (> x (- max-x 3))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (+ x 1) y))
	   (equal? "A" (get-x (+ x 2) y))
	   (equal? "S" (get-x (+ x 3) y)))))

(define (up x y)
  (if (< y 3)
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x x (- y 1)))
	   (equal? "A" (get-x x (- y 2)))
	   (equal? "S" (get-x x (- y 3))))))

(define (down x y)
  (if (> y (- max-y 3))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x x (+ y 1)))
	   (equal? "A" (get-x x (+ y 2)))
	   (equal? "S" (get-x x (+ y 3))))))

(define (diag-ul x y)
  (if (or (< x 3) (< y 3))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (- x 1) (- y 1)))
	   (equal? "A" (get-x (- x 2) (- y 2)))
	   (equal? "S" (get-x (- x 3) (- y 3))))))

(define (diag-ur x y)
  (if (or (> x (- max-x 3)) (< y 3))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (+ x 1) (- y 1)))
	   (equal? "A" (get-x (+ x 2) (- y 2)))
	   (equal? "S" (get-x (+ x 3) (- y 3))))))

(define (diag-dl x y)
  (if (or (< x 3) (> y (- max-y 3)))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (- x 1) (+ y 1)))
	   (equal? "A" (get-x (- x 2) (+ y 2)))
	   (equal? "S" (get-x (- x 3) (+ y 3))))))

(define (diag-dr x y)
  (if (or (> x (- max-x 3)) (> y (- max-y 3)))
      #f
      (and (equal? "X" (get-x x y))
	   (equal? "M" (get-x (+ x 1) (+ y 1)))
	   (equal? "A" (get-x (+ x 2) (+ y 2)))
	   (equal? "S" (get-x (+ x 3) (+ y 3))))))


(define (iter-row x y acc)
  (if (= x max-x)
      acc
      (iter-row (+ 1 x) y (append acc
				  (list (apply + (map
						  (lambda (x) (if x 1 0))
						  (list (up x y)
							(down x y)
							(left x y)
							(right x y)
							(diag-ul x y)
							(diag-ur x y)
							(diag-dl x y)
							(diag-dr x y)))))))))

(define (iter-rows y acc)
  (if (= y max-y)
      acc
      (iter-rows (+ 1 y)
		 (append acc (list (iter-row 0 y '()))))))

(define res (iter-rows 0 '()))


(display (apply +
		(map (lambda (x) (foldl + 0 x))
		     res)))
