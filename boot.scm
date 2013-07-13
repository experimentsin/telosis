;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: boot.scm,v 1.3 1992/08/10 12:51:29 kjp Exp kjp $
;;
;; $Log: boot.scm,v $
;; Revision 1.3  1992/08/10  12:51:29  kjp
;; No change.
;;
;; Revision 1.2  1992/08/09  21:30:09  kjp
;; Feeler distribution version.
;;
;; Revision 1.1  1992/08/04  08:12:58  kjp
;; Initial revision
;;
;; A Booting Telos.
;;
;;   The aim here is a little different to many boot strategies. I'm
;;   not really going for minimal work but for a cleaner final 
;;   specification. 
;;
;;   This boot code defines a kind of mini-telos exploiting as many
;;   assumptions as can be got away with about the full MOP definition.
;;
;;   The functions here come in a few different flavours with 
;;   different naming conventions:
;;
;;     always:name - These can be called safely either during the boot or
;;                   later and are guaranteed to do the right thing either
;;                   way. The macros expand into calls to these guys
;;                   where necessary.
;;
;;     safe:name   - Safe versions of functions which won't cause
;;                   inifinite slot-access recursion.
;;
;;
;;     %func       - Distinctly dodgy functions assuming standard object
;;                   representations. Used during boot only and not
;;                   referenced outside.
;;
;;     boot-name   - Higher level abstractions using % operations. 
;;                   Boot only and unreferenced outside.
;;
;

;; Are we booted yet? The macros all expand into calls to "always"
;; functions. Many of them switch on whether Telos is up or not and
;; either hack it or do the right thing depending. In some cases, they
;; could still hack it after Telos is up given that certain assumptions
;; are met and, indeed, this is a standard technique for making defining
;; ordinary classes and generics faster. I want to test the behaviour
;; of the full definition however so we always go the long way once 
;; we have the long way working.

(define (telos-booted?) #f)

;; A few useful functions:

(define (inconceivable!)
  (display "Inconceivable!")
  (newline)
  (scheme-error "An inconceivable error has occurred! Check for cosmic rays."))

(define (direct-instance? o c) 
  (eq? (class-of o) c))

(define (instance? o c) 
  (subclass? (class-of o) c))

(define (cpl-subclass? c1 c2)
  (memq c2 (safe:class-precedence-list c1)))

(define *no-specifier* (list '-))
(define (no-specifier) *no-specifier*)
(define (no-specifier? o) (eq? o *no-specifier*))

(define (signature-significant-positions sig)
  (positions-matching 
    (lambda (m) (not (no-specifier? m)))
    sig))

(define (signature->selectors sig)
  (list-refs sig (signature-significant-positions sig)))

(define (subsignature? s1 s2)
  (cond
    ((and (null? s1) (null? s2))
      #t)
    ((or (null? s1) (null? s2))
      (telos-error "subsignature? called on non-congruent lists" s1 s2))
    (else
      (and (cpl-subclass? (car s1) (car s2))
	   (subsignature? (cdr s1) (cdr s2))))))

(define (same-signature? s1 s2)
  (cond
    ((and (null? s1) (null? s2))
      #t)
    ((or (null? s1) (null? s2))
      (telos-error "same-signature? called on non-congruent lists" s1 s2))
    (else
      (and (eq? (car s1) (car s2))
	   (same-signature? (cdr s1) (cdr s2))))))

;; Callables support:

(define *callable-table* (make-weak-table eq?))

(define (callable-object-procedure? fn)
  (weak-table-ref *callable-table* fn (lambda () #f)))

(define (procedure->callable-object fn)
  (cdr (weak-table-ref *callable-table* fn (lambda () (cons #f #f)))))

(define (register-callable-object! obj)
  (let*
    ((to-call 
       (lambda args 'rats))
     (handle 
       (lambda args (apply to-call args)))
     (set-to-call! 
       (lambda (fn) (set! to-call fn))))
    (weak-table-set! *callable-table* handle (cons set-to-call! obj))
    handle))

(define (set-callable-object-to-call! handle fn)
  ((car (weak-table-ref *callable-table* handle (lambda () #f))) fn)
  fn)

;
;; Boot safe accessors.
;;
;;   Define primitive accessors and allocators for the system classes.
;;   In theory, given the computed accessor protocol, only the default
;;   discriminator need be trapped for standard classes to break the
;;   infinite recursion.
;;
;

;; Booting methods:

(define-boot-class <method> <method>
  signature range selectors function generic)

;; Assumes signature, function and generic initargs are present...

(define (always:make-method md-class init-list)
  (if (telos-booted?) (apply make (cons md-class init-list))
    (let* ((sig (init-list-ref init-list 'signature inconceivable!))
	   (selectors (signature->selectors sig)))
      (primitive-allocate-initialized
        md-class
	sig
	<object>
	selectors
	(init-list-ref init-list 'function inconceivable!)
	(init-list-ref init-list 'generic inconceivable!)))))

;; Booting generics:

;; Define the cache code here: nasty and slow.

(define (make-cache)
  (cons #f '()))

(define (reset-cache cache)
  (set-car! cache #f)
  (set-cdr! cache '())
  cache)

(define (cache-lookup cache sig)
  (let ((fast (car cache)))
    (if (and fast (same-signature? (car fast) sig)) (cdr fast)
      (let ((slow (cdr cache)))
	(let ((pair (find-match
		      (lambda (pair)
			(same-signature? sig (car pair)))
		      slow)))
	  (if pair (cdr pair) #f))))))

(define (call-cached cache sig fn args)
  (set-car! cache (cons sig fn))
  (apply fn args))

(define (call-caching cache sig fn args)
  (set-cdr! cache (cons (cons sig fn) (cdr cache)))
  (call-cached cache sig fn args))

;; The accessors generated here only work directly on gf objects, not
;; their procedure handles.

(define-boot-class <generic> <generic>
  name signature range selectors method-class methods cache)

;; Embarassing...

(define (safe:generic-cache gf)
  (if (direct-instance? gf <generic>)
    (%generic-cache (procedure->callable-object gf))
    (generic-cache gf)))

;; OK, this is sick and twisted. For purely cosmetic reasons, I want to 
;; be able to define the generic function dispatch protocol with the
;; rest of the MOP. To avoid duplicating code, MOP generics are booted
;; with a thunk for a discriminator ("thunk for brains!" - passable insult).
;; When the thunk is called for the first time (after the world is in place),
;; the discriminator is computed and then re-installed as the function to 
;; be called.
;;
;; There is no real need for this. The "safe" definitions of the generic
;; lookup code could be defined prior to processing the MOP definition.
;; This is an easy change to make should this approach become unworkable.

(define (always:make-generic gf-class init-list)
  (if (telos-booted?) (apply make (cons gf-class init-list))
    (let* ((sig (init-list-ref init-list 'signature inconceivable!))
	   (sel (signature->selectors sig))
	   (obj (primitive-allocate-initialized
		  gf-class
		  (init-list-ref init-list 'name inconceivable!)
		  sig
		  <object>
		  sel
		  <method>
		  (init-list-ref init-list 'methods (lambda () '()))
		  (make-cache))))
      (let* ((handle (register-callable-object! obj))
	     (real-disc #f)
	     (disc-thunk
	      (lambda args
		(unless real-disc
 		  (let* ((meths (%generic-methods obj))
			 (lookup (safe:compute-method-lookup-function 
				   handle
				   sig
				   (%generic-methods obj)))
			 (disc (safe:compute-discriminating-function
				 handle
				 sig
				 meths
				 lookup)))
		    (set-callable-object-to-call! handle disc)
		    (set! real-disc disc)))
		(apply real-disc args))))
	(set-callable-object-to-call! handle disc-thunk)
	handle))))

(define (always:add-method gf md)
  (if (telos-booted?) (add-method gf md)
    (let ((gf-obj (procedure->callable-object gf)))
      (unless (equal? (signature-significant-positions 
		        (%generic-signature gf-obj))
		      (signature-significant-positions
		        (%method-signature md)))
	(scheme-error "boot problem: method/gf sigs don't match in add-method"
		      (%generic-name gf-obj)))
      (%set-generic-methods! gf-obj (cons md (%generic-methods gf-obj)))
      (%set-method-generic! md gf)
      gf)))

(define (always:generic-method-class gf)
  (if (telos-booted?) (generic-method-class gf)
    (%generic-method-class (procedure->callable-object gf))))

;; Booting classes:

(define *class-size* 7)

(define-boot-class <slot-description> <slot-description>
  name position initfunction initable? reader writer)

(define-boot-class <class> <metaclass>
  name precedence-list slot-descriptions initargs
  direct-superclasses direct-subclasses instance-size)

(define-boot-accessors <metaclass> <metaclass>
  default-slot-description-class *class-size*)

(define (always:allocate-class meta init-list)
  (if (telos-booted?) (allocate meta init-list)
    (primitive-allocate meta *class-size*)))

(define (always:allocate-metaclass meta init-list)
  (if (telos-booted?) (allocate meta init-list)
    (primitive-allocate meta (+ *class-size* 1))))

(define (boot-slot-description class init-list pos)
  (let* ((name (init-list-ref init-list 'name #f))
	 (read-name (make-reader-name (%class-name class) name))
	 (write-name (make-writer-name (%class-name class) name)))
    (primitive-allocate-initialized
      <slot-description>
      name
      pos
      (init-list-ref init-list 'initfunction  slot-unbound-value)
      (init-list-ref init-list 'initable? (lambda () #t))
      (cond
        ((eq? (primitive-class-ref class) <callable-class>)
	  (generic-lambda ((o class))
	    evaluated-name read-name
	    method (((o class)) 
		    (primitive-ref (procedure->callable-object o) pos))))
	(else
	  (generic-lambda ((o class))
	    evaluated-name read-name
	    method (((o class)) 
		    (primitive-ref o pos)))))
      (cond
        ((eq? (primitive-class-ref class) <callable-class>)
	  (generic-lambda ((o class) value)
	    evaluated-name write-name
	    method (((o class) value)
		    (primitive-set! (procedure->callable-object o) pos value)
		    value)))
	(else
	  (generic-lambda ((o class) value)
	    evaluated-name write-name
	    method (((o class) value)
		    (primitive-set! o pos value)
		    value)))))))

(define (boot-slot-descriptions class slots n)
  (if (null? slots) '()
    (cons 
     (boot-slot-description class (first slots) n)
     (boot-slot-descriptions class (rest slots) (+ n 1)))))

(define (always:initialize-class class init-list)
  (if (telos-booted?) (initialize class init-list)
    (let ((name   (init-list-ref init-list 
				 'name (lambda () '<anonymous>)))
	  (supers (init-list-ref init-list 
				 'direct-superclasses #f))
	  (slots  (init-list-ref init-list
				 'direct-slot-descriptions #f))
	  (keys (init-list-ref init-list
			       'direct-initargs (lambda () '()))))

      (define (cpl supers)
	(reverse-weed (map-appending %class-precedence-list supers) eq?))

      (define (super-slots supers)
	(if (null? supers) '() (%class-slot-descriptions (first supers))))

      (%set-class-name! class name)
      (%set-class-precedence-list! class 
				   (cons class (cpl supers)))
      (%set-class-slot-descriptions! class 
				    (append (super-slots supers)
					    (boot-slot-descriptions 
					      class
					      slots
					      (length (super-slots supers)))))
      (%set-class-initargs! class
			    (append keys 
				    (map-appending %class-initargs supers)))
      (%set-class-direct-superclasses! class supers)
      (%set-class-direct-subclasses! class '())
      (for-each
        (lambda (super)
	  (%set-class-direct-subclasses!
	    super
	    (cons class (%class-direct-subclasses super))))
	supers)
      (%set-class-instance-size! class 
				 (length (%class-slot-descriptions class)))

      ;; meta
      (when (eq? (primitive-class-ref class) <metaclass>)
        (%set-metaclass-default-slot-description-class! class 
							<slot-description>))

      class)))

(define (always:find-slot-description c name)
  (if (telos-booted?) (find-slot-description c name)
    (find-match (lambda (o) (eq? name (%slot-description-name o)))
		(%class-slot-descriptions c))))

(define (always:slot-description-reader slotd)
  (if (telos-booted?) (slot-description-reader slotd)
    (%slot-description-reader slotd)))

(define (always:slot-description-writer slotd)
  (if (telos-booted?) (slot-description-writer slotd)
    (%slot-description-writer slotd)))

;; Preallocated classes...

(define <object>          (always:allocate-class #f '()))
(define <class>           (always:allocate-metaclass #f '()))
(define <metaclass>       (always:allocate-metaclass #f '()))
(define <abstract-class>  (always:allocate-metaclass #f '()))
(define <primitive-class> (always:allocate-metaclass #f '()))
(define <callable-class>  (always:allocate-metaclass #f '()))

(primitive-class-set! <object>          <abstract-class>)
(primitive-class-set! <class>           <metaclass>)
(primitive-class-set! <metaclass>       <metaclass>)
(primitive-class-set! <abstract-class>  <metaclass>)
(primitive-class-set! <primitive-class> <metaclass>)
(primitive-class-set! <callable-class>  <metaclass>)

(define <slot-description> (always:allocate-class <class> '()))
(define <generic>          (always:allocate-class <callable-class> '()))
(define <method>           (always:allocate-class <class> '()))

;; We now have enough to define the MOP neatly.

;; eof
