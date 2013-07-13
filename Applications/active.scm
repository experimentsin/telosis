(define-class <active-> ()

  ((act?    accessor active-slot-description-act?
	    initform (lambda (o val) #t))
   (action  accessor active-slot-description-action))

  initargs (act? action)
  metaclass <abstract-class>

)

(define-class <active-supporting-> () () metaclass <abstract-class>)

(define-method 
  (compute-primitive-writer-using-slot-description (slotd <active->) c sds)

  (let ((write (call-next-method))
	(act? (active-slot-description-act? slotd))
	(action (active-slot-description-action slotd)))
    (lambda (o value)
      (write o (if (act? o value) (action o value) value)))))

;; ---

;; With a MOP management protocol, I should be able to compute the details of
;; this instantiable metaclass automatically.

(define-class <active-slotd> (<active-> <slot-description>) ())
(define-metaclass <active-supporting-class> (<active-supporting-> <class>) ())

(define-method (compute-defined-slot-description-class (c <active-supporting->)
						      spec)
  (if (init-list-ref spec 'action (lambda () #f))
    <active-slotd>
    <slot-description>))

;; ---

(define-class <wop> (<object>)

  ((normal accessor wop-normal
	   initform 'norm)

   (weird accessor wop-weird
	  initform 'weird
	  act?     (lambda (o v) (number? v))
	  action   (lambda (o v) (display "[number]") (* v 2)))

   (always accessor wop-always
	   initform 'always
	   action   (lambda (o v) (display "[always]") (list v))))

  metaclass <active-supporting-class>)

(define i (make <wop>))
;; [always] -> i=#<wop>

(set-wop-weird! i 'a)
;; -> a

(set-wop-weird! i 23)
;; [number] -> 46

(set-wop-always! i #t)
;; [always] -> (#t)

;; eof
