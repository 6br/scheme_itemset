#!/usr/local/bin/gosh

;;; itemset.scm - Item Set Mining
;;;
;;; Copyright (c) 2015 6br
;;;
;;; Item set mining implementation for scheme

(define (backtrack p i t theta m option)
	(display p)
	(newline)
	(if (null? option)
		(backtrack-lcm-iter p i t theta (+ i 1) m option)
		(backtrack-lcm-iter-occ p option t theta m))
	)

(define (backtrack-iter p i t theta iter m)
	(if (<= theta (computeSupport (union p (list iter)) t 0))
		(backtrack (union p (list iter)) iter (computeA p t '()) theta m)
		(if (< iter m) (backtrack-iter p i t theta (+ iter 1) m ) '())
		)
	)

(define (backtrack-lcm-iter c i t theta iter m opt)
	(if (< m iter) '()
		(if (member iter c) (backtrack-lcm-iter c i t theta (+ iter 1) m opt) 
			(if (> theta (computeSupport (union c (list iter)) t 0))
				(backtrack-lcm-iter c i t theta (+ iter 1) m opt)
				;;(if (not(fast-ppc? (computeA (union c (list iter)) t '()) (- iter 1 ) i))
				(if (not (call_ppc? c (computeClojure (union c (list iter)) t '()) iter))
					(backtrack-lcm-iter c i t theta (+ iter 1) m opt)
					(begin (backtrack (computeClojure (union c (list iter)) t '()) iter (computeA (union c (list iter)) t '()) theta m opt)
								 (backtrack-lcm-iter c i t theta (+ iter 1) m opt)
								 ))
				)
			)
		)
	)

(define (backtrack-lcm-iter-occ c i t theta m)
	(if (null? i) '()
		(if (member (car i) c) (backtrack-lcm-iter-occ c (cdr i) t theta m) 
			(if (> theta (computeSupport (union c (list (car i))) t 0))
				(backtrack-lcm-iter-occ c (cdr i) t theta m)
				;;(if (not(fast-ppc? (computeA (union c (list (car i))) t '()) (- (car i) 1 )))
				(if (not (call_ppc? c (computeClojure (union c (list (car i))) t '()) (car i)))
					(backtrack-lcm-iter-occ c (cdr i) t theta m)
					(begin (backtrack (computeClojure (union c (list (car i))) t '()) (car i) (computeA (union c (list (car i))) t '()) theta m i)
								 (backtrack-lcm-iter-occ c (cdr i) t theta m)
								 ))
				)
			)
		)
	)
(define (computeA p t n)
	(if (null? t) n
		(computeA p (cdr t) (if (equal? p (intersect p (car t))) (cons (car t) n) n))
		)
	)

(define (computeSupport p t n)
	(if (null? t) n
		(computeSupport p (cdr t) (if (equal? p (intersect p (car t))) (+ n 1) n))
		)
	)

(define (computeClojure p t c)
	(if (null? t) c (if (not (equal? p (intersect p (car t)))) (computeClojure p (cdr t) c)
										(cond ((and (null? c) (null? (car t)) '()))
													((and (null? c) (not (null? (car t)))) (computeClojure p (cdr t) (car t)))
													((and (not (null? c)) (null? (intersect c (car t)))) '())
													((and (not (null? c)) (not (null? (intersect c (car t))))) (computeClojure p (cdr t) (intersect c (car t)))) 
													)
										)
		)
	)

(define (call_ppc? cf cs i)
	(ppc? cf cs i (length cs))
	;;(ppc? (sort cf <) (sort cs <) i (length cs))
	)

(define (ppc? cf cs i iter)
	#|(newline)
	(display cf)
	(display cs)
	(display i)
	(newline)|#
	(if (= iter 0) #t 
		(if (null? cf) (if (<= i (car cs)) #t #f)
			(if (<= i (car cs)) #t (if (not (equal? (car cf) (car cs))) #f (ppc? (cdr cf) (cdr cs) i (- iter 1)))) 
			)))


(define (fast-ppc? c i iter)
	#|(display c)
	(display i)
	(display iter)
	(newline)|#
	(if (or (= i 0) (= i (+ iter 1)) (= i iter)) #t
		(if (fast-ppc-iter? c i) #f (fast-ppc? c (- i 1) iter))
		)
	)

;; iがcの全てに 含まれているなら#t, 含まれないなら#f
(define (fast-ppc-iter? c i)
	(if (null? c) #t
		(if (member i (car c)) (fast-ppc-iter? (cdr c) i) #f)
		))

(define (union x y)
	(cond ((null? x) y)
				((member (car x) y)
				 (union (cdr x) y))
				(else
					(cons (car x) (union (cdr x) y)))))

(define intersect
	(lambda (set1 set2)
		(cond
			((null? set1)(quote ()))
			((member (car set1) set2)
			 (cons (car set1)
						 (intersect (cdr set1) set2)))
			(else (intersect (cdr set1) set2)))))

(define (call_backtrack theta t m option)
	(if (> theta (length t))
		'()
		(backtrack (computeClojure '() t '()) 0 t theta m option)
		)
	)

(define (type)
	(let loop ((ls '()))
		(let ((x (read-line)))
			(cond ((eof-object? x) ls)
						(else (loop (cons (map string->number (string-split x #\Space)) ls)) )))
		))

(define (occ array result num)
	(if (= 0 num) result
		(occ array (cons (reverse (occ-iter array '() num 1)) result) (- num 1)))
	)

(define (get ls n)
	(if (zero? n)
		(car ls)
		(get (cdr ls) (- n 1))))

(define (occ-rebase array result master)
	(if (null? array) result
		(occ-rebase (cdr array) (union (get master (- (car array) 1)) result) master))
	)

(define (occ-iter array result num last)
	(if (null? array) result
		(if (member num (car array))
			(occ-iter (cdr array) (cons last result) num (+ last 1))
			(occ-iter (cdr array) result num (+ last 1))))
	)

(define (flatten x)
	(cond ((null? x) '())
				((not (pair? x)) (list x))
				(else (append (flatten (car x))
											(flatten (cdr x))))))

(define (max-list ls)
	(fold (lambda (x a) (if (< a x) x a)) (car ls) (cdr ls)))

(define (mini-occ array)
	(mini-occ-iter (flatten array) (max-list (flatten array)) '())
	)
(define (mini-occ-iter array i result)
	(if (< i 0) result
		(if (member i array)
			(mini-occ-iter array (- i 1) (cons i result))
			(mini-occ-iter array (- i 1) result))))

(define (main args)
	;;(display (max-list (flatten (with-input-from-file (cadr args) type))))
	;;(display (occ (list (car (occ (with-input-from-file (cadr args) type) '() (string->number (cadddr args))))) '() 500 ))
	(if (null? (cdr args)) (test)
		(let ((inputfile (with-input-from-file (cadr args) type)))
			(if (null? (cdddr args))
				(call_backtrack (string->number (caddr args)) inputfile (max-list (flatten inputfile)) '())
				(call_backtrack (string->number (caddr args)) inputfile (max-list (flatten (occ inputfile '() (length inputfile)))) (mini-occ inputfile)))
			;;(call_backtrack (string->number (caddr args)) inputfile (max-list (flatten (occ inputfile '() (length inputfile)))) (occ inputfile '() (length inputfile))))
			))
	)

(define (test)
	(display (union '((1 2)(3 4)(5 6)) '((9 10) (7 8) (5 6))))
	(newline)
	(display (computeA '(1 2) '((1 2 3) (1 2) (3 7)) '()))
	(newline)
	(display (intersect '(1 2 3) '(1 2)))
	(newline)
	(display (computeClojure '() '((1 2 5 6 7 9) (2 3 4 5) (1 2 7 8 9) (1 7 9) (2 7 9) (2)) '() ))
	(newline)
	(call_backtrack 1 '((1 2 3) (1 2)) 9 '())
	(newline)
	(let ((inputfile '((1 2 5 6 7 9) (2 3 4 5) (1 2 7 8 9) (1 7 9) (2 7 9) (2))))
		(call_backtrack 1 '((1 2 5 6 7 9) (2 3 4 5) (1 2 7 8 9) (1 7 9) (2 7 9) (2)) 9 (mini-occ inputfile)))
	(newline)
	(call_backtrack 1 '((1 2 5 6 7 9) (2 3 4 5) (1 2 7 8 9) (1 7 9) (2 7 9) (2)) 9 '())
	(newline)
	(call_backtrack 1 '((1 2 5 6 7 9) (1 2 7 8 9) (1 7 9)) 9 '())
	(newline)
	)
