;;; Copyright (C) 1991 Aubrey Jaffer.

; `Load' as defined in Revised^3.99 Report on the Algorithmic
; Language Scheme [Draft August 31, 1989] opens a back door to eval
; owing to its dynamic nature:

;  (eval <expression>)					procedure
;  (eval! <expression>)					procedure

; Eval returns the value of <expression> in the current top level
; environment.  Eval! returns an unspecified value.  Side effects of
; <expression> will effect the top level environment.
; (program-vicinity) will be incorrect during the evaluation of
; <expression>.

(define (provided? x) #f)

(define eval:global-return #f)

(define eval:depth-cntr 0)

(define eval:temp-filenames '())

(define slib:eval!
  (let ((eval:load load))
    (lambda (frob)
      (set! eval:depth-cntr (+ 1 eval:depth-cntr))
      (let ((filename
	     (cond
	      ((> eval:depth-cntr (length eval:temp-filenames))
	       (set! eval:temp-filenames
		     (cons 
		      (if (provided? 'tmpnam) (tmpnam)
			  (string-append
			   "eval_"
			   (number->string (+ 100 eval:depth-cntr))))
		      eval:temp-filenames))
	       (car eval:temp-filenames))
	      (else (list-ref eval:temp-filenames
			      (- (length eval:temp-filenames)
				 eval:depth-cntr))))))
	(call-with-output-file filename
	  (lambda (file)
	    (write frob file)))
	(eval:load filename))
      (set! eval:depth-cntr (- eval:depth-cntr 1)))))

(define (slib:eval frob)
  (slib:eval! (list 'set! 'eval:global-return frob))
  eval:global-return)

(define eval! slib:eval!)
(define eval slib:eval)
