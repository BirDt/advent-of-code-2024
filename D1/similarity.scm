
(import
  (chicken io)
  (chicken sort)
  (chicken string))

(define pairs (map (lambda (x)
		     (map string->number (string-split x)))
		   (call-with-input-file "input" (lambda (port) (read-lines port)))))

(define left-list (sort (map car pairs) <))
(define right-list (sort (map cadr pairs) <))

(define (find-in-right x)
  (* x (apply + (map (lambda (y)
		       (if (= x y)
			   1
			   0))
		     right-list))))

(define similarities (map find-in-right left-list))

(display (apply + similarities))
