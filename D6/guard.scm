
(import
  (chicken io)
  (chicken string))

(define maze (map (lambda (x) (string-chop x 1))
		  (call-with-input-file "input" (lambda (port) (read-lines port)))))

(define max-y (length maze))

(define max-x (length (car maze)))

(define (get-y y)
  (list-ref maze y))

(define (get-x x y)
  (if (or (= y max-y) (= x max-x) (< y 0) (< x 0))
      "."
      (list-ref (get-y y) x)))

;; Position of the guard - x, y, direction (UP DOWN LEFT RIGHT)
(define guard '(0 0 UP))

(define directions (string-chop "^v<>" 1))

(define (get-direction x)
  (cond
   ((equal? x "^") 'UP)
   ((equal? x "v") 'DOWN)
   ((equal? x "<") 'LEFT)
   ((equal? x ">") 'RIGHT)
   (else (error "INVALID DIRECTION"))))

(define (get-guard-character)
  (let ((c (caddr guard)))
    (cond
     ((equal? c 'UP) "^")
     ((equal? c 'DOWN) "v")
     ((equal? c 'LEFT) "<")
     ((equal? c 'RIGHT) ">"))))

(define (inc-guard)
  (cond
   ((equal? 'UP (caddr guard)) (list (car guard) (- (cadr guard) 1)))
   ((equal? 'DOWN (caddr guard)) (list (car guard) (+ (cadr guard) 1)))
   ((equal? 'LEFT (caddr guard)) (list (- (car guard) 1) (cadr guard)))
   ((equal? 'RIGHT (caddr guard)) (list (+ (car guard) 1) (cadr guard)))))

(define (turn-guard)
  (let ((c (caddr guard)))
    (set! (caddr guard)
      (cond
       ((equal? c 'UP) 'RIGHT)
       ((equal? c 'DOWN) 'LEFT)
       ((equal? c 'LEFT) 'UP)
       ((equal? c 'RIGHT) 'DOWN)))))

(define (find-guard-row x y)
(if (= x max-x)
    #f
    (if (member (get-x x y) directions)
	(list x (get-x x y))
	(find-guard-row (+ 1 x) y))))

(define (find-guard-iter y)
  (if (= y max-y)
      #f
      (let ((check-row (find-guard-row 0 y)))
	(if check-row
	    (set! guard (list (car check-row) y (get-direction (cadr check-row))))
	    (find-guard-iter (+ 1 y))))))

(define (find-guard)
  (find-guard-iter 0))

(find-guard)

(define (guard-step)
  (set! (list-ref (list-ref maze (cadr guard)) (car guard)) "X")
  (let ((next-step (inc-guard)))
    (if (or (> 0 (car next-step)) (> 0 (cadr next-step))
	    (= max-x (car next-step)) (= max-y (cadr next-step)))
	'DONE
	(if (equal? (get-x (car next-step) (cadr next-step)) "#")
	    (begin
	      (turn-guard)
	      (guard-step))
	    (begin
	      (set! guard (list (car next-step) (cadr next-step) (caddr guard)))
	      (set! (list-ref (list-ref maze (cadr next-step)) (car next-step)) (get-guard-character))
	      guard)))))

(define (iter-guard)
  (if (equal? (guard-step) 'DONE)
      #t
      (iter-guard)))

(iter-guard)

(define (step? x)
  (equal? x "X"))

(define res
  (let ((x (flatten maze)))
    (length (compress (map step? x) x))))

(display res)
