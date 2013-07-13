;; Approximation of Harry's mixins. We get most of this for free from the 
;; default simple MI behaviour.

(define-metaclass <base-class> (<class>) () predicate base-class?)

(define-method (compatible-superclass? (sub <base-class>)
				       (sup <class>))
  #t)

(define-method (compatible-superclasses? (c <base-class>) 
					 supers)
  (when (null? supers)
    (telos-error "Base classes must have at least one superclass" c supers))

  (let* ((len (length supers))
	 (mix (rest supers))
	 (base (first supers)))

    (unless (all? mixin-class? mix)
      (telos-error "Non-mixin class in mixin superclass position" c mix))
    (when (mixin-class? base)
      (telos-error "Mixin class in base class position" c base))

    (call-next-method)))

(define-metaclass <mixin-class> (<abstract-class>) () predicate mixin-class?)

(define-method (allocate (mc <mixin-class>) init-list)
  ;; The inherited method from <abstract-class> outlaws this but I want a
  ;; more specific error message.
  (telos-error "Attempted to allocate a direct instance of a mixin class" mc))

(define-method (compatible-superclass? (sub <mixin-class>)
				       (sup <class>))
  (eq? sup <object>))

(define-method (compatible-superclass? (sub <mixin-class>)
				       (sup <mixin-class>))
  #t)

;; Macros...

(define-telos-syntax (define-base-class name supers slots . class-ops)
  `(define-class ,name ,(reverse supers)
     ,slots metaclass <base-class> ,@class-ops))

(define-telos-syntax (define-mixin-class name supers slots . class-ops)
  `(define-class ,name ,supers 
     ,slots metaclass <mixin-class> ,@class-ops))

;; ---

;; Tests (ugh)...

  ;;; Define a base-class Point:

  (define-base-class <Point> (<object>)
    ((x initform 0 accessor point-x)
     (y initform 0 accessor point-y))
    initargs (x y))

  ;;; Define a mixin-class colored (independent from Point):

  (define-mixin-class <colored> ()
    ((color initform 'black))
    initargs (color))

  (define-generic (color-of (obj <colored>))
    method (((obj <object>)) 'gray)
    method (((obj <colored>)) (slot-value obj 'color)))

  (define-generic (change-color (obj <colored>) color)
    method (((obj <object>) color) 
	       (telos-warning
		 "Can't change the color of an uncolored object"
		 obj color)
	       'gray)
    method (((obj <colored>) color)
	      (set-slot-value! obj 'color color)))

  ;;; Define a mixin-class green-colored:

  (define-mixin-class <green-colored> (<colored>)
    (((specialise color)
        initform 'green)))

  (define-method (change-color (obj <green-colored>) color)
    (unless (eq? color 'green)
      (telos-warning "Wrong color for a green-colored object" obj color))
    'green)

  ;;; Define some special Point classes:

  (define-base-class <colored-Point> (<colored> <Point>) ())
  
  (define-base-class <green-Point> (<green-colored> <Point>) ())

