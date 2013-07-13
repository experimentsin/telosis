;; Get some more sensible names to preserve our sanity...

(define <callable-object-class> <callable-class>)

;; First, define a new subclass of <metaclass>!
;; 
;; Abusing our limited MI support here. This new class describes metaclasses
;; whose metainstances (classes) are callable...

(define-metaclass 
  <callable-class-metaclass> (<metaclass> <callable-object-class>)
  ())

(define-method (compatible-superclass? (sub <callable-class-metaclass>)
				       (sup <metaclass>))
  #t)

;; Using this, we define a new callable class metaclass...

(define-metaclass <callable-class> (<callable-object> <class>) ()
  metaclass <callable-class-metaclass>)

(define-method (compute-callable-object-procedure (c <callable-class>))
  (lambda args
    (apply make (cons c args))))

;; We make our first callable class...

(define-class <wop> (<object>)
  (a b c)
  initargs (a b c)
  metaclass <callable-class>)

;; Prepare to be appalled...

(define i (make <wop> 'a 1 'b 2 'c 3))

;; is now equivalent to...

(define j (<wop> 'a 1 'b 2 'c 3))

