
(import
  (chicken io)
  (chicken sort)
  (chicken string))

(define pairs (map (lambda (x)
		     (map string->number (string-split x)))
		   (call-with-input-file "input" (lambda (port) (read-lines port)))))

(define left-list (sort (map car pairs) <))
(define right-list (sort (map cadr pairs) <))

(define (difference x y)
  (abs (- x y)))

(define distances (map difference left-list right-list))

(display (apply + distances))
