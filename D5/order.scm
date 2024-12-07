
(import
  (chicken io)
  (chicken sort)
  (chicken string)
  comparse
  srfi-14)

(define input (call-with-input-file "input" (lambda (port) (read-string #f port))))

(define digit
  (in char-set:digit))

(define digits
  (as-string (one-or-more digit)))

(define rule
  (sequence* ((a digits)
	      (_ (is #\|))
	      (b digits)
	      (_ (is #\newline)))
	     (result (list 'RULE (map string->number (list a b))))))

(define page-list
  (sequence* ((x (one-or-more (sequence* ((x digits)
					  (_ (is #\,)))
					 (result (string->number x)))))
	      (y digits)
	      (_ (maybe (is #\newline))))
	     (result (append x (list (string->number y))))))

(define top-level
  (sequence* ((x (one-or-more rule))
	      (_ (is #\newline))
	      (y (one-or-more page-list)))
	     (result (append x y))))

(define parse-result (parse top-level input))

(define rules '())

(define page-lists '())

(define (categorise x)
  (if (equal? (car x) 'RULE)
      (set! rules (append rules (list (list (caadr x) (cadadr x)))))
      (set! page-lists (append page-lists (list x)))))

(map categorise parse-result)

(define (find-rules x)
  (map cadr (compress (map (lambda (y) (= (car y) x)) rules) rules)))

(define (subset? x y)
  (let ((z (map (lambda (j) (memq j y)) x)))
    (= 0 (length (compress (map (lambda (j) (equal? #f j)) z) z)))))

(define (valid-instance? i lst rules)
  (if (= i (- (length lst) 1))
      #t
      (subset? (sort (list-tail lst (+ i 1)) >) (sort rules >))))

(define (validity-iter v i x)
  (if (or (not v) (= (length x) i))
      v
      (let* ((y (list-ref x i))
	     (member-rules (find-rules y)))
	(validity-iter (valid-instance? i x member-rules)
		       (+ 1 i)
		       x))))

(define (test-validity x)
  (validity-iter #t 0 x))

(define valid-lists (compress (map test-validity page-lists) page-lists))

(define (get-middle lst)
  (list-ref lst (/ (- (length lst) 1) 2)))

(display (apply + (map get-middle valid-lists)))
