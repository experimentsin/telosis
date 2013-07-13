;; Old Telos initialisation semantics...

(define-class <old-slotd> (<slot-description>)

  ((initargs accessor slot-description-initargs
	     initform '()))

  initargs (initargs))

(define-metaclass <old-slotd-supporting-class> (<class>) ())

(define-method 
  (compute-defined-slot-description-class (c <old-slotd-supporting-class>)
					  spec)
  <old-slotd>)

(define-method
  (compute-specialized-slot-description (c <old-slotd-supporting-class>)
					spec
					others)
  (let* ((sd (call-next-method))
	 (all-initargs
	   (weed
	     (append
	       (init-list-ref spec 'initargs (lambda () '()))
	       (map-appending slot-description-initargs others))
	     eq?)))
    (set-slot-description-initargs! sd all-initargs)
    sd))

(define-method 
  (initialize-slot-using-class (c <old-slotd-supporting-class>)
			       sd
			       o 
			       init-list)
  ;; stuff
)

;; etc...