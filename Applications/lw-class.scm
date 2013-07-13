;; Lightweight classes.

(define-metaclass <lw-class> (<class>) ())
(define-class <lw-object> (<object>) () metaclass <lw-class>)

(define-method (compatible-superclass? (c <lw-class>) (sup <abstract-class>))
  (eq? sup <object>))

(define-method (compatible-superclass? (c <lw-class>) (sup <class>))
  #f)

(define-method (compute-constructor (c <lw-class>) keys)
  (let* ((size (class-instance-size c))
	 (init-sds (map (lambda (key) (find-slot-description c key)) keys))
	 (init-posns (map slot-description-position sds)))
    (lambda vals
      (let ((o (primitive-allocate c size)))
	(for-each
	  (lambda (pos val)
	    (primitive-set! o pos val))
	  init-posns
	  vals)
	o))))


    
    