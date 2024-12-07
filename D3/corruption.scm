
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

(define any-char
  (sequence* ((_ (in char-set:printing)))
	     (result '())))

(define top-level
  (zero-or-more (any-of
		 mul
		 any-char)))

(display (apply + (map (lambda (x) (if (null? x)
				       0
				       (* (car x) (cadr x))))
		       (parse top-level input))))
