;; Can't remember the semantics of CLOS' method combination. This is just
;; an example to show how that sort of thing might me done.

(define-class <mc-generic> (<generic>) () metaclass <callable-class>)

(define-class <mc-method> (<method>)

  ((qualifier accessor method-qualifier
	      initform ':primary))

  initargs (qualifier)
  metaclass <class>)
  
(define-method (compute-combining-method-function (gf <mc-generic>)
						  applicable)

  (define (extract qualifier mds)
    (filter
      (lambda (md)
	(eq? (method-qualifier md) qualifier))
      mds))
      
  (let ((primary (extract ':primary applicable))
	(before  (extract ':before applicable))
	(after   (extract ':after applicable)))

    (when (null? primary)
      (telos-error "no applicable primary methods" gf args))
    
    (lambda args
      (unless (null? before)
	(apply-method (first before) (rest before) args))
      (let ((result (apply-method (first primary) (rest primary) args)))
	(unless (null? after)
	  (apply-method (first after) (rest after) args))
	result))))

(define-method (equivalent-method? (gf <mc-generic>) md1 md2)
  (and (call-next-method)
       (eq? (method-qualifier md1)
	    (method-qualifier md2))))
				   
;; Syntax...

(define-telos-syntax (define-mc-generic name-vars . options)
  `(define-generic ,name-vars ,@options 
     class <mc-generic>
     method-class <mc-method>))

(define-telos-syntax (define-mc-method . forms)
  (let ((qualifier ':primary))
    (when (symbol? (first forms))
      (set! qualifier (first forms))
      (set! forms (rest forms)))
    `(define-method qualifier ',qualifier ,@forms)))

;; Test

(define-mc-generic (wop (x <object>)))

;; Objs...

(define-mc-method (wop (x <object>)) 
  (display "[object-sandwiched]")
  (list 'object x))

(define-mc-method :before (wop (x <object>))
  (display "[object-before]"))

(define-mc-method :after (wop (x <object>))
  (display "[object-after]"))

;; Ints...

(define-mc-method (wop (x <integer>)) 
  (display "[integer-sandwiched]")
  (list 'integer (call-next-method)))

(define-mc-method :before (wop (x <integer>))
  (display "[integer-before]")
  (call-next-method))

(define-mc-method :after (wop (x <integer>))
  (display "[integer-after]")
  (call-next-method))

(wop 'a)
;; [object-before][object-sandwiched][object-after] -> (object a)

(wop 1)
;; [integer-before][object-before][integer-sandwiched][object-sandwiched]
;; [integer-after][object-after] -> (integer (object 1))

;; eof
