
(import
  (chicken io)
  (chicken string)
  comparse
  srfi-14)

(define input (call-with-input-file "input" (lambda (port) (read-string #f port))))

(define digit
  (in char-set:digit))

(define digits
  (as-string (one-or-more digit)))

(define mul
  (sequence* ((_ (char-seq "mul("))
	      (v1 digits)
	      (_ (is #\,))
	      (v2 digits)
	      (_ (is #\))))
	     (result (list (string->number v1)
			   (string->number v2)))))

(define do-inst
  (sequence* ((_ (char-seq "do()")))
	     (result 'START)))

(define dont-inst
  (sequence* ((_ (char-seq "don't()")))
	     (result 'STOP)))

(define any-char
  (sequence* ((_ (in char-set:printing)))
	     (result '())))

(define top-level
  (zero-or-more (any-of
		 mul
		 do-inst
		 dont-inst
		 any-char)))

(define c #t)

(display (apply + (map (lambda (x) (if (null? x) 
				       0					
				       (cond
					((equal? x 'STOP) (set! c #f) 0)
					((equal? x 'START) (set! c #t) 0)
					(else (if c (* (car x) (cadr x)) 0)))))			
		       (parse top-level input))))

