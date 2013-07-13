(define-metaclass <structure-class> (<class>) ())

(define-class <structure> (<object>) () metaclass <structure-class>)

(define-method (compatible-superclasses? (c <structure-class>) supers)
  (and (= (length supers) 1) (call-next-method)))

(define-method (compatible-superclass? (c <structure-class>) 
				       (super <structure-class>))
  #t)

(define-method (compatible-superclass? (c <structure-class>) 
				       (super <class>))
  (eq? super <object>))

(define-method (compute-slot-reader (c <structure-class>)
				    (sd <slot-description>)
				    sds)
  (let ((pos (slot-description-position sd)))
    (lambda (o) (primitive-ref o pos))))

(define-method (ensure-slot-reader (c <structure-class>)
				   (sd <slot-description>)
				   sds
				   reader)
  (let ((pos (slot-description-position sd))
	(name (slot-description-name sd)))
    (unless (all?
	      (lambda (super)
		(= pos (slot-description-position 
			 (find-slot-description c name))))
	      (class-direct-superclasses c))
      (telos-error "slot moved in subclass of a <structure-class>" c name)))
  reader)

(define-method (compute-slot-writer (c <structure-class>)
				    (sd <slot-description>)
				    sds)
  (let ((pos (slot-description-position sd)))
    (lambda (o v) (primitive-set! o pos v) v)))

(define-method (ensure-slot-writer (c <structure-class>)
				   (sd <slot-description>)
				   sds
				   reader)
  (let ((pos (slot-description-position sd))
	(name (slot-description-name sd)))
    (unless (all?
	      (lambda (super)
		(= pos (slot-description-position 
			 (find-slot-description c name))))
	      (class-direct-superclasses c))
      (telos-error "slot moved in subclass of a <structure-class>" c name)))
  reader)

(define-telos-syntax (define-structure name super slots . stuff)
  (let ((supers (if (null? super) '(<structure>) `(,super)))
	(init (map
	        (lambda (spec)
		  (cond
		    ((symbol? spec) spec)
		    ((symbol? (first spec)) (first spec))
		    (else (second (first spec)))))
		slots)))
    `(define-class ,name ,supers ,slots
       initargs ,init
       metaclass <structure-class>
       ,@stuff)))

(define-structure <wop> () ((a accessor wop-a)))
(define-structure <ack> <wop> ((b accessor ack-b)))

