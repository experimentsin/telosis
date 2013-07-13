;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: mop.scm,v 1.5 1992/08/17 22:07:43 kjp Exp kjp $
;;
;; $Log: mop.scm,v $
;; Revision 1.5  1992/08/17  22:07:43  kjp
;; Fixed superclass of <vector> - was <primitive-class>!
;;
;; Revision 1.4  1992/08/11  03:25:28  kjp
;; Fixed subclass? - a #f/() confusion.
;;
;; Revision 1.3  1992/08/10  12:50:51  kjp
;; Compute-specialized-slot-description-class uses class of first inherited.
;;
;; Revision 1.2  1992/08/09  21:30:09  kjp
;; Feeler distribution version.
;;
;; Revision 1.1  1992/08/04  08:13:12  kjp
;; Initial revision
;;
;; Full MOP Definition.
;;
;;   The initial boot sequence leaves us in a state where all of the
;;   MOP classes have been pre-allocated and have had their class-of
;;   relationships initialised. 
;;
;;   We can now use our much simplified "booting TELOS" to define the
;;   MOP cleanly from the top of the heirarchy down as a TELOS 
;;   program. The only wrinkles are that we must use 
;;   define-allocated-class instead of define-class for the 
;;   pre-allocated classes and that the potential infinite recursion
;;   in the generic dispatch code must be broken by calling a 
;;   special "safe" version defined above.
;;
;;   It's the nature of the thing that binding references weave out
;;   and about throughout this definition. You ain't got nothin' 'til
;;   you got it all as they say.
;;
;

;
;; Top of the class heirarchy:
;;
;;   Top of the tree. Slotless, superless and abstract.
;;
;

(define-allocated-class <object> () () metaclass <abstract-class>)

;; Generic object initialisation protocol:

(define (make class . init-list)
  (initialize (allocate class init-list) init-list))

(define (make-with-init-list class init-list)
  (initialize (allocate class init-list) init-list))

(define-generic (initialize (o <object>) init-list))

;; Default behaviour:

(define-method (initialize (o <object>) init-list)
  (initialize-using-class (class-of o) o init-list))

;
;; Primitive classes and class-of:
;;
;;   The Scheme classes over which we have no control. Generate likely
;;   looking classes to correspond with the type heirarchy and fix
;;   class-of to (eventually) return the right thing.
;;
;;   Not strictly MOP but needs doing somewhere.
;;
;

(define-class <primitive-object> (<object>) () metaclass <primitive-class>)

;; Numeric classes:

(define-class <exact-> () () metaclass <abstract-class>)
(define-class <inexact-> () () metaclass <abstract-class>)

(define-class <number> (<primitive-object>) () metaclass <abstract-class>)
  (define-class <integer> (<exact-> <number>) () metaclass <primitive-class>)
  (define-class <rational> (<exact-> <number>) () metaclass <primitive-class>)
  (define-class <real> (<inexact-> <number>) () metaclass <primitive-class>)

;; Other "atomic" structures:

(define-class <character> (<primitive-object>) () metaclass <primitive-class>)
(define-class <string> (<primitive-object>) () metaclass <primitive-class>)
(define-class <symbol> (<primitive-object>) () metaclass <primitive-class>)
(define-class <procedure> (<primitive-object>) () metaclass <primitive-class>)
(define-class <input-port> (<primitive-object>) () metaclass <primitive-class>)
(define-class <output-port> (<primitive-object>) () metaclass <primitive-class>)

;; Aggregates:

(define-class <list> (<primitive-object>) () metaclass <primitive-class>)
  (define-class <null> (<list>) () metaclass <primitive-class>)
  (define-class <pair> (<list>) () metaclass <primitive-class>)
(define-class <vector> (<primitive-object>) () metaclass <primitive-class>)

;; The class-of computer:

(define (class-of o)
  (if (primitive-object? o) (primitive-class-ref o) 
    (cond
      ((integer? o) <integer>)
      ((rational? o) <rational>)
      ((real? o) <real>)
      ((null? o) <null>) ;; Must go before number? because of a bug in xscheme
      ((number? o) <number>)
      ((char? o) <character>)
      ((symbol? o) <symbol>)
      ((string? o) <string>)
      ((input-port? o) <input-port>)
      ((output-port? o) <output-port>)
      ((pair? o) <pair>)
      ((vector? o) <vector>)
      ((callable-object-procedure? o)
        (primitive-class-ref (procedure->callable-object o)))
      ((procedure? o) <procedure>)
      (else <primitive-object>))))

;; To be tidy, all meta-objects will inherit from <mop-object>

(define-class <mop-object> (<object>) () metaclass <abstract-class>)

;
;; Slot Descriptions.
;;
;;   Just an implementation tool for classes and have little in the
;;   way of a protocol of their own that isn't class-controlled.
;;
;

(define-allocated-class <slot-description> (<mop-object>) 

  ((name          accessor slot-description-name)
   (position      accessor slot-description-position)
   (initfunction  accessor slot-description-initfunction)
   (initable?     accessor slot-description-initable?)
   (reader        accessor slot-description-reader)
   (writer        accessor slot-description-writer))

  initargs (name initfunction initable? reader writer)

  predicate slot-description?
  metaclass <class>)

;; Protocol:

(define-generic (describe-same-slot? (sd1 <slot-description>)
				     (sd2 <slot-description>)))

;; Default behaviour:

;; Readers and writers uniquely identify...

(define-method (describe-same-slot? (sd1 <slot-description>)
				    (sd2 <slot-description>))
  (eq? (slot-description-reader sd1)
       (slot-description-reader sd2)))

;
;; Classes.
;;
;;   A hunky protocol for classes as you might expect. We'll do it top
;;   down, so starting with initialisation and then each of the sub
;;   protocols in turn followed by their default behaviour in the
;;   same order.
;;
;

(define-allocated-class <class> (<mop-object>)

  ((name                accessor class-name)
   (precedence-list     accessor class-precedence-list)
   (slot-descriptions   accessor class-slot-descriptions)
   (initargs            accessor class-initargs)
   (direct-superclasses accessor class-direct-superclasses)
   (direct-subclasses   accessor class-direct-subclasses)
   (instance-size       accessor class-instance-size))

  initargs (name 
	    direct-superclasses
	    direct-initargs
	    direct-slot-descriptions
	    inherited-slot-description-specializers)

  predicate class?
  metaclass <metaclass>)

;
;; Protocol:
;

;; Misc:

(define-generic (subclass? (c1 <class>) (c2 <class>)))
(define-generic (find-slot-description (c <class>) name))

(define-generic (compute-predicate (c <class>)))
(define-generic (compute-constructor (c <class>) args))

;; Allocation:

(define-generic (allocate (c <class>) init-list))

;; Instance initialisation:

(define-generic (initialize-using-class (c <class>) o init-list))
(define-generic (valid-initargs-using-class? (c <class>) init-list))
(define-generic (initialize-slot-using-class (c <class>) slotd o init-list))

;; Initialisation:

;;   Superclass relationships:

(define-generic (compatible-superclasses? (c <class>) supers))
(define-generic (compatible-superclass? (sub <class>) (sup <class>)))
(define-generic (compute-class-precedence-list (c <class>) supers))

(define-generic (add-subclass (c <class>) (sub <class>)))

;;   Initarg processing:

(define-generic (compute-inherited-initargs (c <class>) supers))
(define-generic (compute-initargs (c <class>) inherited direct))

;;   Slot processing:

(define-generic (compute-inherited-slot-descriptions (c <class>) supers))

(define-generic (compute-slot-descriptions (c <class>) new specs inherited))
					  
(define-generic (compute-defined-slot-description (c <class>) spec))
(define-generic (compute-defined-slot-description-class (c <class>) spec))

(define-generic (compute-specialized-slot-description (c <class>)
						      spec 
						      sds))
(define-generic (compute-specialized-slot-description-class (c <class>)
							    spec 
							    sds))

;;     Slot combination:

(define-generic (compute-initfunction (sd <slot-description>) others))

;;   Class finalisation:

(define-generic (finalize-slot-descriptions (c <class>) slotds))
(define-generic (finalize-slot-description (c <class>) 
					   (sd <slot-description>)
					   slotds))
(define-generic (compute-slot-position (c <class>) 
				       (sd <slot-description>)
				       slotds))

(define-generic (compute-class-instance-size (c <class>) slotds))
(define-generic (compute-slot-size (c <class>) (sd <slot-description>)))

;;     Accessor generation and update:

;;     Readers:

(define-generic 
  (compute-slot-reader (c <class>) 
		       (sd <slot-description>)
		       sds))
(define-generic 
  (ensure-slot-reader (c <class>)
		      (sd <slot-description>)
		      sds
		      fn))
(define-generic 
  (compute-primitive-reader-using-slot-description (sd <slot-description>)
						   class
						   sds))
(define-generic 
  (compute-primitive-reader-using-class (c <class>)
					sd
					sds))
					

;;     Writers:

(define-generic 
  (compute-slot-writer (c <class>)
		       (sd <slot-description>)
		       sds))
(define-generic 
  (ensure-slot-writer (c <class>) 
		      (sd <slot-description>)
		      sds
		      fn))
(define-generic 
  (compute-primitive-writer-using-slot-description (sd <slot-description>)
						   class
						   sds))
(define-generic 
  (compute-primitive-writer-using-class (c <class>)
					sd
					sds))

;
;; Default behaviour:
;;
;;   The behaviour defined here actually supports simple multiple 
;;   inheritance where there is no join apart from <object> in the
;;   precedence lists of the superclasses. 
;;
;;   Comment: Given the protocol we define, I think we should support 
;;   at least some token MI scheme like this by default rather than 
;;   provide an MI supporting protocol with a SI-only implementation. 
;;   A bit of a tease don't you think?
;;
;

;; Misc:

(define-method (subclass? (c1 <class>) (c2 <class>))
  (if (memq c2 (class-precedence-list c1)) #t #f))

(define-method (find-slot-description (c <class>) name)
  (find-match
    (lambda (sd)
      (eq? name (slot-description-name sd)))
    (class-slot-descriptions c)))

(define-method (compute-constructor (c <class>) keys)
  ;; Efficient or what?
  (let ((reqd (length keys)))
    (lambda args
      (when (not (= (length args) reqd))
	(telos-error "wrong number of args for constructor" c keys args))
      (apply make c (map-appending list keys args)))))

(define-method (compute-predicate (c <class>))
  (generic-lambda ((o <object>))
    evaluated-name (symbol-append (class-name c) '?)
    method (((o <object>)) #f)
    method (((o c)) #t)))

;; Allocation:

(define-method (allocate (c <class>) init-list)
  (primitive-allocate c (class-instance-size c)))

;; Instance initialisation:

(define-method (initialize-using-class (c <class>) o init-list)
  ;; Validate initargs:
  (unless (valid-initargs-using-class? c init-list)
    (telos-error "invalid initarg provided for initialisation"
		 o init-list (class-initargs c)))
  ;; Fill slots:
  (for-each
    (lambda (sd)
      (initialize-slot-using-class c sd o init-list))
    (class-slot-descriptions (class-of o)))
  o)

(define-method (valid-initargs-using-class? (c <class>) init-list)
  (let ((valid (class-initargs c)))
    (let walk ((l init-list))
      (cond
        ((null? l) #t)
	((memq (first l) valid) (walk (rest^2 l)))
	(else #f)))))

(define-method (initialize-slot-using-class (c <class>)
					    slotd
					    o 
					    init-list)

  (define (default)
    (let ((inf (slot-description-initfunction slotd)))
      (unless (slot-unbound-value? inf)
	((slot-description-writer slotd) o (inf)))))
    
  (if (slot-description-initable? slotd)
    (let ((val (init-list-ref init-list 
			      (slot-description-name slotd)
			      undefined-value)))
      (if (undefined-value? val) 
	(default)
	((slot-description-writer slotd) o val)))
    (default)))

;; Initialisation:

(define-method (initialize (c <class>) init-list)
  (call-next-method)
  (let 
    ((name   
       (init-list-ref init-list 
		      'name 
		      (lambda () '<anonymous>)))
     (supers 
       (init-list-ref init-list 
		      'direct-superclasses
		      (lambda () (list <object>))))
     (slots
       (init-list-ref init-list 
		      'direct-slot-descriptions
		      (lambda () '())))

     (mods
       (init-list-ref init-list 
		      'inherited-slot-description-specializers 
		      (lambda () '())))

     (keys
       (init-list-ref init-list 
		      'direct-initargs
		      (lambda () '()))))

    ;; Name...
    (set-class-name! c name)
    
    ;; Inheritance validity check...
    (unless (compatible-superclasses? c supers)
      (telos-error "incompatible superclasses for a class with this metaclass"
		   c supers))

    ;; Compute and install the cpl...
    (set-class-precedence-list! 
      c 
      (compute-class-precedence-list c supers))

    ;; Compute and install slots...
    (let ((inherited (compute-inherited-slot-descriptions c supers)))
      (let ((effective (compute-slot-descriptions c slots mods inherited)))
	(set-class-slot-descriptions! c effective)
	;; Finalise...
	(finalize-slot-descriptions c effective)
	(set-class-instance-size! 
	  c 
	  (compute-class-instance-size c effective))))

    (let ((inherited (compute-inherited-initargs c supers)))
      (set-class-initargs! c (compute-initargs c inherited keys)))

    (set-class-direct-subclasses! c '())
    (set-class-direct-superclasses! c supers)

    ;; Register ourselves with our parents...
    (for-each
      (lambda (super)
	(add-subclass super c))
      supers)

    c))

;;   Superclass relationships:

(define-method (compatible-superclasses? (c <class>) supers)
  (all? (lambda (super) (compatible-superclass? c super)) supers))

(define-method (compatible-superclass? (c <class>) (super <class>)) 
  (subclass? (class-of c) (class-of super)))

(define-method (compute-class-precedence-list (c <class>) direct-supers)

  (define (walk supers seen seen-object?)
    (if (null? supers) (if seen-object? (list <object>) '())
      (let ((super (first supers)))
	(cond
	  ((eq? super <object>)
	    (walk (rest supers) 
		  seen 
		  #t))
	  ((memq super seen)
	    (telos-warning "diamond in cpl" super c direct-supers)
	    (walk (rest supers) 
		  seen
		  seen-object?))
	  (else
	    (cons super (walk (rest supers) 
			      (cons super seen)
			      seen-object?)))))))

  (cons c
	(walk (map-appending class-precedence-list direct-supers) 
	      '() 
	      #f)))

(define-method (add-subclass (super <class>) (sub <class>))
  (set-class-direct-subclasses! 
    super
    (cons sub (class-direct-subclasses super)))
  super)

;;   Initarg processing:

(define-method (compute-inherited-initargs (c <class>) supers)
  (weed (map-appending class-initargs supers) eq?))

(define-method (compute-initargs (c <class>) inherited direct)
  (weed (append inherited direct) eq?))

;;   Slot processing:

(define-method (compute-inherited-slot-descriptions (c <class>) supers)
  (let* ((all (map-appending class-slot-descriptions supers))
	 (names (weed (map slot-description-name all) eq?)))
    (map
      (lambda (name)
	(filter (lambda (sd) (eq? name (slot-description-name sd))) all))
      names)))

(define-method (compute-slot-descriptions (c <class>) defs mods inherited)

  (let ((defined-sds
	  (map
	    (lambda (def)
	      (compute-defined-slot-description c def))
	    defs))

	(inherited-sds
	 (map
	   (lambda (inherited-sds)
	     (let* ((name (slot-description-name (first inherited-sds)))
		    (modifier 
		     (find-match 
		       (lambda (spec)
			 (eq? (init-list-ref spec 'name #f) name))
		       mods)))
	       (compute-specialized-slot-description
	         c 
		 (if modifier modifier '())
		 inherited-sds)))
	   inherited)))

    ;; Checks...
	
    (for-each
      (lambda (defined-sd)
	(let ((name (slot-description-name defined-sd)))
	  (when (find-match 
		  (lambda (sd) (eq? (slot-description-name sd) name))
		  inherited-sds)
	    (telos-error "attempted to define an existing slot name"
			 c name inherited))))
      defined-sds)

    (for-each
      (lambda (modifier)
	(let ((name (init-list-ref modifier 'name #f)))
	  (unless (find-match 
		    (lambda (sd) (eq? (slot-description-name sd) name))
		    inherited-sds)
	    (telos-error "attempted to specialise a non-existent slot"
			 c name modifier))))
      mods)

    (append inherited-sds defined-sds)))

;; Definitions:

;; Actually, now this is neater, this would be the natural place to
;; compute and install fresh accessors if slot position weren't an issue.

(define-method (compute-defined-slot-description (c <class>) def)
  (make-with-init-list (compute-defined-slot-description-class c def) def))

(define-method (compute-defined-slot-description-class (c <class>) spec)
  (metaclass-default-slot-description-class (class-of c)))

;; Modifications:

(define-method (compute-specialized-slot-description (c <class>) spec sds)
  ;; (unless (= (length sds) 1)
  ;;  (telos-error "inherited slot clash" c sds))
  (let ((class (compute-specialized-slot-description-class c spec sds))
	(most-spec (first sds)))
    (make class 
	  'name 
	    (init-list-ref 
	      spec 
	      'name (lambda () 
		      (slot-description-name most-spec)))
	    'initfunction 
	      (init-list-ref 
	        spec 
		'initfunction (lambda () 
				(slot-description-initfunction most-spec)))
	    'initable?
	      (init-list-ref 
	        spec 
		'initable? (lambda () 
			     (slot-description-initable? most-spec)))
	    'reader
	      (slot-description-reader most-spec)

	    'writer
	      (slot-description-writer most-spec))))

(define-method (compute-specialized-slot-description-class (c <class>)
							   spec
							   others)
  (class-of (first others)))

;;   Class finalisation:

(define-method (finalize-slot-descriptions (c <class>) sds)
  (for-each
    (lambda (sd) (finalize-slot-description c sd sds))
    sds)
  c)

(define-method (finalize-slot-description (c <class>) 
					  (sd <slot-description>)
					  sds)
  (set-slot-description-position! 
    sd
    (compute-slot-position c sd sds))

  ;; Compute readers if they aren't there already...

  (when (slot-unbound-value? (slot-description-reader sd))
    (set-slot-description-reader!
      sd
      (compute-slot-reader c sd sds)))
  (when (slot-unbound-value? (slot-description-writer sd))
    (set-slot-description-writer!
      sd
      (compute-slot-writer c sd sds)))

  ;; Make sure they work...

  (ensure-slot-reader c sd sds (slot-description-reader sd))
  (ensure-slot-writer c sd sds (slot-description-writer sd))

  c)

(define-method (compute-slot-position (c <class>) 
				      (sd <slot-description>)
				      sds)
  (position sd sds eq?))

(define-method (compute-class-instance-size (c <class>) sds)
  (foldl (lambda (count sd) (+ count (compute-slot-size c sd))) 0 sds))

(define-method (compute-slot-size (c <class>) 
				  (sd <slot-description>))
  1)

;;   Accessor processing:

;;     Readers:

(define-method (compute-slot-reader (c <class>) 
				    (sd <slot-description>)
				    sds)
  (make <generic> 
	'signature (list c) 
	'name (make-reader-name (class-name c) (slot-description-name sd))))

(define-method (ensure-slot-reader (c <class>)
				   (sd <slot-description>)
				   sds
				   reader)
  (let ((fn (compute-primitive-reader-using-slot-description sd c sds)))
    (define-method (reader (i c)) (fn i))))

(define-method 
  (compute-primitive-reader-using-slot-description (sd <slot-description>)
						   c
						   sds)
  (compute-primitive-reader-using-class c sd sds))

(define-method (compute-primitive-reader-using-class (c <class>) 
						     sd
						     sds)
  (let ((pos (compute-slot-position c sd sds)))
    (lambda (o) (primitive-ref o pos))))

;;     Writers:

(define-method (compute-slot-writer (c <class>)
				    (sd <slot-description>)
				    sds)
  (make <generic>
	'signature (list c (no-specifier))
	'name (make-writer-name (class-name c) (slot-description-name sd))))

(define-method (ensure-slot-writer (c <class>) 
				   (sd <slot-description>)
				   sds
				   writer)
  (let ((fn (compute-primitive-writer-using-slot-description sd c sds)))
    (define-method (writer (i c) o) (fn i o))))

(define-method 
  (compute-primitive-writer-using-slot-description (sd <slot-description>)
						   c
						   sds)
  (compute-primitive-writer-using-class c sd sds))

(define-method (compute-primitive-writer-using-class (c <class>)
						     sd
						     sds)
  (let ((pos (compute-slot-position c sd sds)))
    (lambda (o v) (primitive-set! o pos v))))

;
;; Other metaclasses.
;;
;;   Just fill-in the gaps here. The only weirdness is the inclusion of
;;   the <callable-class> metaclass that we need to hide as best we can
;;   the fact that the generic function you have in your hand is in fact
;;   a normal Scheme procedure.
;;
;

;; Metaclasses (the self-instantiating class):

(define-allocated-class <metaclass> (<class>)
  
  ((default-slot-description-class 
     accessor metaclass-default-slot-description-class
     initform <slot-description>))

  initargs (default-slot-description-class)
  predicate metaclass?
  metaclass <metaclass>)

(define-method (initialize (m <metaclass>) init-list)
  (call-next-method))

;; Abstract classes:

(define-allocated-class <abstract-class> (<class>) 

  () 
  
  predicate abstract-class?
  metaclass <metaclass>)

;; A few nasty overrides:

(define-method (allocate (c <abstract-class>) init-list)
  (telos-error 
    "attempted to allocate an instance of an abstract class"
    c
    init-list))

(define-method (compatible-superclass? (sub <class>)
				       (super <abstract-class>))
  #t)

;; Primitive classes:

(define-allocated-class <primitive-class> (<class>)
  
  ()

  predicate primitive-class?
  metaclass <metaclass>)

(define-method (allocate (c <primitive-class>) init-list)
  (telos-error 
    "attempted to allocate an instance of a primitive class"
    c
    init-list))

;; Callable classes:

(define-allocated-class <callable-class> (<class>)

  ()

  predicate callable-class?
  metaclass <metaclass>)

(define-class <callable-object> (<object>) 
  
  ;; ((procedure accessor callable-object-procedure))
  ()

  metaclass <callable-class>
  predicate callable-object?)

;; Protocol:

(define-generic (compute-callable-object-procedure (o <callable-object>)))
(define-generic (set-callable-object-procedure! (o <callable-object>) proc))

;; Behaviour:

(define-method (allocate (o <callable-class>) init-list)
  (let* ((o (call-next-method))
	 (handle (register-callable-object! o)))
    handle))

(define-method (initialize (o <callable-object>) init-list)
  (call-next-method)
  (let ((proc (compute-callable-object-procedure o)))
    (set-callable-object-procedure! o proc))
  o)

(define-method (compute-primitive-reader-using-class (c <callable-class>)
						     slotd
						     slotds)
  (let ((reader (call-next-method)))
    (lambda (handle) (reader (procedure->callable-object handle)))))

(define-method (compute-primitive-writer-using-class (c <callable-class>)
						     slotd
						     slotds)
  (let ((writer (call-next-method)))
    (lambda (handle value)
      (writer (procedure->callable-object handle) value))))

(define-method (set-callable-object-procedure! (o <callable-object>) proc)
  (set-callable-object-to-call! o proc)
  proc)

;
;; Methods:
;;
;;   Much like slot descriptions in the sense of their subsidiarity to
;;   generics. See the generic function protocol.
;;
;

(define-allocated-class <method> (<mop-object>)

  ((signature accessor method-signature)
   (range     accessor method-range
	      initform <object>)
   (selectors accessor method-selectors)
   (function  accessor method-function)
   (generic   accessor method-generic))

  initargs (signature range function generic)

  predicate method?
  metaclass <class>)

(define-method (initialize (m <method>) init-list)
  (call-next-method)
  (set-method-selectors! m (signature->selectors (method-signature m)))
  m)

;
;; Generics:
;;
;;   Where the action is. Because of the callable object hackery
;;   and my quest for neatness, this uses noddy MI so as to be 
;;   recognised as a MOP object while still inheriting the 
;;   behaviour of a callable.
;;
;

(define-allocated-class <generic> (<mop-object> <callable-object>)

  ((name         accessor generic-name)
   (signature    accessor generic-signature)
   (range        accessor generic-range
		 initform <object>)
   (selectors    accessor generic-selectors)
   (method-class accessor generic-method-class
		 initform <method>)
   (methods      accessor generic-methods
	         initform '())
   (cache        accessor generic-cache
	         initform (make-cache)))

  initargs (name signature methods method-class)

  predicate generic?
  metaclass <callable-class>)

;; Protocol:

;; Initialisation:

(define-generic (finalize-generic (gf <generic>)))
(define-generic (compute-discriminating-function (gf <generic>)
						 gf-signature
						 gf-methods
						 md-lookup))

(define-generic (compute-method-lookup-function (gf <generic>)
						gf-signature
						gf-methods))

(define-generic (compute-combining-method-function (gf <generic>)
						   applicable-mds))
(define-generic (method-more-specific? (gf <generic>) md1 md2))

;; Update:

(define-generic (find-method (gf <generic>) sig))
(define-generic (add-method (gf <generic>) (md <method>)))
(define-generic (compatible-method? (gf <generic>) (md <method>)))
(define-generic (equivalent-method? (gf <generic>) md1 md2))
(define-generic (remove-method (gf <generic>) (md <method>)))

;; Default behaviour:

;; Initialisation:

(define-method (initialize (gf <generic>) init-list)
  (call-next-method)
  (let
    ((name 
       (init-list-ref init-list 
		      'name 
		      (lambda () 'anonymous)))
     (signature
       (init-list-ref init-list
		      'signature 
		      (lambda ()
			(telos-error "no domain specified for generic"
				     init-list))))
     (range
       (init-list-ref init-list
		      'range
		      (lambda () <object>)))
     (method-class
       (init-list-ref init-list 
		      'method-class
		      (lambda () <method>)))
     (methods
       (init-list-ref init-list
		      'methods
		      (lambda () '()))))

    (set-generic-name! gf name)
    (set-generic-signature! gf signature)
    (set-generic-range! gf range)
    (set-generic-selectors! gf (signature->selectors (generic-signature gf)))
    
    (set-generic-method-class! gf method-class)

    (for-each
      (lambda (md)
	(add-method gf md))
      methods)

    (finalize-generic gf))

  gf)

(define-method (compute-callable-object-procedure (o <generic>))
  (lambda args
    (telos-error "generic uninitialised" o)))

(define-method (finalize-generic (gf <generic>))
  (let* ((signature (generic-signature gf))
	 (methods (generic-methods gf))
	 (lookup (compute-method-lookup-function gf signature methods))
	 (disc (compute-discriminating-function gf signature methods lookup)))
    (set-callable-object-procedure! gf disc))
  (reset-cache (generic-cache gf))
  gf)


(define-safe-method (compute-method-lookup-function (gf <generic>)
						    signature 
						    methods)
  (let ((required (length signature))
	(selector-posns (signature-significant-positions signature)))
    (lambda args
      (let ((arg-count (length args)))
	(when (< arg-count required)
	  (telos-error "insufficient arguments for method lookup" gf args))
	(let* ((selectors (list-refs args selector-posns))
	       (signature (map class-of selectors))
	       (applicable (filter
			     (lambda (m)
			       (subsignature? signature
					      (safe:method-selectors m)))
			     methods)))
	  (sort applicable
		(lambda (m1 m2)
		  (safe:method-more-specific? gf m1 m2))))))))

(define-safe-method (compute-combining-method-function (gf <generic>)
						       applicable)
  (let ((most (first applicable))
	(next (rest applicable)))
    (lambda args
      (apply-method most next args))))

(define-safe-method (compute-discriminating-function (gf <generic>)
						     signature
						     methods
						     lookup)
  (let ((required (length signature))
	(selector-posns (signature-significant-positions signature))
	(cache (safe:generic-cache gf)))
    (lambda args
      (let ((arg-count (length args)))
	(when (< arg-count required)
	  (telos-error "insufficient arguments for generic" 
		       gf args required))
	(let* ((selectors (list-refs args selector-posns))
	       (signature (map class-of selectors)))
	  (let ((fn (cache-lookup cache signature)))
	    (if fn (call-cached cache signature fn args)
	      (let ((applicable (apply lookup args)))
		(when (null? applicable)
		  (telos-error "no applicable methods" gf args))
		(call-caching 
		  cache
		  signature 
		  (safe:compute-combining-method-function gf applicable)
		  args)))))))))

(define-safe-method (method-more-specific? (gf <generic>) md1 md2)
  (subsignature? (safe:method-selectors md1)
		 (safe:method-selectors md2)))

;; Update:

(define-method (find-method (gf <generic>) sig)
  (let ((match (find-match 
		 (lambda (m)
		   (same-signature? sig (method-signature m)))
		 (generic-methods gf))))
    match))

(define-method (add-method (gf <generic>) (md <method>))
  (unless (compatible-method? gf md)
    (telos-error "attempted to add incompatible method" gf md))
  (let ((old-method (find-match
		      (lambda (old)
			(equivalent-method? gf old md))
		      (generic-methods gf))))
    (when old-method (remove-method gf old-method)))
  (set-generic-methods! gf
			(cons md (generic-methods gf)))
  (set-method-generic! md gf)
  (finalize-generic gf)
  gf)

;; The idea of giving generic functions signatures was to ensure that only
;; valid subclasses could specialise. A problem arises if we want to use
;; a mixin style however since the mixins will not be such subclasses -
;; only the evential instantiable class derived from them.
;;
;; This "practical" subsignature test counts abstract classes as being 
;; valid subclasses of any class.

(define (practical-subsignature? l1 l2)
  (cond
    ((null? l1) #t)
    ((or (instance? (first l1) <abstract-class>)
	 (subclass? (first l1) (first l2)))
      (practical-subsignature? (rest l1) (rest l2)))
    (else #f)))

(define-method (compatible-method? (gf <generic>) (md <method>))
  (let ((gf-sig (generic-signature gf))
	(md-sig (method-signature md)))
    (and (= (length gf-sig) (length md-sig))
	 (equal? (signature-significant-positions gf-sig)
		 (signature-significant-positions md-sig))
	 (practical-subsignature? (method-selectors md)
				  (generic-selectors gf))
	 (subclass? (method-range md)
		    (generic-range gf)))))

(define-method (equivalent-method? (gf <generic>) m1 m2)
  (same-signature? (method-signature m1)
		   (method-signature m2)))

(define-method (remove-method (gf <generic>) (md <method>))
  (set-generic-methods! gf
			(filter 
			  (lambda (m) (not (eq? m md)))
			  (generic-methods gf)))
  (set-method-generic! md (slot-unbound-value))
  (finalize-generic gf)
  gf)

;; That's it.

(define (telos-booted?) #t)

;; eof

