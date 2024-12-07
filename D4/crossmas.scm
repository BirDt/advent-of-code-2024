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

(define (rot-one x y)
  (if (and (> x 0) (< x (- max-x 1))
	   (> y 0) (< y (- max-y 1)))
      (and (equal? "A" (get-x x y))
	   (equal? "M" (get-x (- x 1) (- y 1)))
	   (equal? "M" (get-x (- x 1) (+ y 1)))
	   (equal? "S" (get-x (+ x 1) (- y 1)))
	   (equal? "S" (get-x (+ x 1) (+ y 1))))
      #f))

(define (rot-two x y)
  (if (and (> x 0) (< x (- max-x 1))
	   (> y 0) (< y (- max-y 1)))
      (and (equal? "A" (get-x x y))
	   (equal? "S" (get-x (- x 1) (- y 1)))
	   (equal? "M" (get-x (- x 1) (+ y 1)))
	   (equal? "S" (get-x (+ x 1) (- y 1)))
	   (equal? "M" (get-x (+ x 1) (+ y 1))))
      #f))

(define (rot-three x y)
  (if (and (> x 0) (< x (- max-x 1))
	   (> y 0) (< y (- max-y 1)))
      (and (equal? "A" (get-x x y))
	   (equal? "S" (get-x (- x 1) (- y 1)))
	   (equal? "S" (get-x (- x 1) (+ y 1)))
	   (equal? "M" (get-x (+ x 1) (- y 1)))
	   (equal? "M" (get-x (+ x 1) (+ y 1))))
      #f))

(define (rot-four x y)
  (if (and (> x 0) (< x (- max-x 1))
	   (> y 0) (< y (- max-y 1)))
      (and (equal? "A" (get-x x y))
	   (equal? "M" (get-x (- x 1) (- y 1)))
	   (equal? "S" (get-x (- x 1) (+ y 1)))
	   (equal? "M" (get-x (+ x 1) (- y 1)))
	   (equal? "S" (get-x (+ x 1) (+ y 1))))
      #f))


(define (iter-row x y acc)
  (if (= x max-x)
      acc
      (iter-row (+ 1 x) y (append acc
				  (list (apply + (map
						  (lambda (x) (if x 1 0))
						  (list (rot-one x y)
							(rot-two x y)
							(rot-three x y)
							(rot-four x y)))))))))

(define (iter-rows y acc)
  (if (= y max-y)
      acc
      (iter-rows (+ 1 y)
		 (append acc (list (iter-row 0 y '()))))))

(define res (iter-rows 0 '()))


(display (apply +
		(map (lambda (x) (foldl + 0 x))
		     res)))
