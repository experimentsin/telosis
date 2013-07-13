;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: macros.scm,v 1.3 1992/08/10 12:38:16 kjp Exp kjp $
;;
;; $Log: macros.scm,v $
;; Revision 1.3  1992/08/10  12:38:16  kjp
;; Added next-method? and made the no next method error more informative.
;;
;; Revision 1.2  1992/08/09  21:30:09  kjp
;; Feeler distribution version.
;;
;; Revision 1.1  1992/08/04  08:12:43  kjp
;; Initial revision
;;
;; Telos Macros.
;;
;;   Straightforward old-style macro definitions for the syntax of Telos.
;;   So that we can use the same macros while booting as after the boot is
;;   complete, MOP calls from the expansions go through always: functions
;;   guaranteed to work in both states.
;;
;

;; Requires: utils.scm and macro support.

;; Note: Error checking in all of these is practically non-existent. You're
;;       more likely to get "car called on ()"-like messages than anything
;;       useful. Better later.

;; A couple of non-telos specific macros:

(define-telos-syntax (when x . stuff)
  `(if ,x (begin ,@stuff) 'when-fail))

(define-telos-syntax (unless x . stuff)
  `(if (not ,x) (begin ,@stuff) 'unless-fail))

;; Generics and methods...

(define *next-method-args* (gensym))
(define *next-method-list* (gensym))

(define (lambda-list-formals ll)
  (cond 
   ((null? ll) '())
   ((not (pair? ll)) ll)
   ((pair? (car ll)) (cons (car (car ll)) (lambda-list-formals (cdr ll))))
   (else
     (cons (car ll) (lambda-list-formals (cdr ll))))))

(define (lambda-list-signature ll)
  (cond 
    ((not (pair? ll)) '())
    ((pair? (car ll)) (cons (car (cdr (car ll))) 
			    (lambda-list-signature (cdr ll))))
    (else
      (cons '(no-specifier) (lambda-list-signature (cdr ll))))))

(define-telos-syntax (method-function-lambda gf formals . body)
  `(lambda (,*next-method-args* ,*next-method-list* . ,formals)
     (define (call-next-method)
       (when (null? ,*next-method-list*)
	 (telos-error "no next method" ,gf ,*next-method-args*))
       (apply-method (first ,*next-method-list*)
		     (rest ,*next-method-list*)
		     ,*next-method-args*))
     (define (next-method?) (not (null? ,*next-method-list*)))
     ,@body))


(define-telos-syntax (method-lambda gf class inits ll . body)
  `(always:make-method 
     ,class
     (list
       'generic ,gf
       'signature (list ,@(lambda-list-signature ll))
       'function (method-function-lambda
		  ,gf ,(lambda-list-formals ll) ,@body)
       ,@inits)))

(define *local-generic* (gensym))

(define (process-generic-options opts k)
  (let ((methods '())
	(others '())
	(gf-class (undefined-value)))
    (for-each-key
      (lambda (key val)
	(case key
	  ((method)
	    (set! methods
		  (cons `(define-method ,(cons *local-generic* (car val))
			   ,@(cdr val))
			methods)))
	  ((name)
	    (set! others
		  `('name ',val ,@others)))
	  ((->)
	    (set! others
		  `('range ,val ,@others)))
	  ((evaluated-name)
	    (set! others
		  `('name ,val ,@others)))
	  ((class)
	    (set! gf-class val))
	  (else
	    (set! others
		  (cons `(quote ,key) (cons val others))))))
      opts)
    (k (if (undefined-value? gf-class) '<generic> gf-class)
       methods
       others)))

;; No generic-labels - rely on Scheme's local function definition semantics.

(define-telos-syntax (generic-lambda ll . options)
  (process-generic-options options
    (lambda (gf-class method-defs init-list)
      `(let ((,*local-generic*  
	       (always:make-generic ,gf-class
		 (list 
		   'signature (list ,@(lambda-list-signature ll))
		   ,@init-list))))
	 ,@method-defs
	 ,*local-generic*))))

(define-telos-syntax (define-generic name-vars . options)
  `(define ,(first name-vars)
     (generic-lambda ,(rest name-vars)
       name ,(first name-vars)
       ,@options)))

(define (process-method-form form return)
  (define (extract-initargs form sofar)
    (cond
      ((pair? (first form))
        (return 
	  (caar form)
	  (reverse sofar)
	  (cdar form)
	  (cdr form)))
      (else
        (extract-initargs (rest^2 form) 
			  (cons (second form)
				(cons `',(first form) sofar))))))
  (extract-initargs form '()))

(define-telos-syntax (define-method . form)
  (process-method-form form
    (lambda (gf initargs ll body)
      `(always:add-method
	 ,gf
	 (method-lambda 
	   ,gf 
	   (always:generic-method-class ,gf)
	   ,initargs ,ll ,@body)))))

;; Dodgy double evaluations of args here...

(define-telos-syntax (call-method md next . args)
  `((safe:method-function ,md) (list ,@args) ,next ,@args))

(define-telos-syntax (apply-method md next args)
  `(apply (safe:method-function ,md) (cons ,args (cons ,next ,args))))

;; Class definition:

(define-telos-syntax (define-class name supers slot-ops . class-ops)
  `(begin
     (define ,name (always:allocate-class 
		     ,(init-list-ref class-ops 
				     'metaclass (lambda () '<class>)) '()))
     (define-allocated-class ,name ,supers ,slot-ops ,@class-ops)))

(define-telos-syntax (define-metaclass name supers slot-ops . class-ops)
  `(begin
     (define ,name (always:allocate-class 
		     ,(init-list-ref class-ops 
				     'metaclass (lambda () '<metaclass>)) '()))
     (define-allocated-class ,name ,supers ,slot-ops ,@class-ops)))

(define-telos-syntax (define-abstract-class name supers slot-ops . class-ops)
  `(begin
     (define ,name (always:allocate-class 
		     ,(init-list-ref class-ops 
				     'metaclass (lambda () '<abstract-class>))
		     '()))
     (define-allocated-class ,name ,supers ,slot-ops ,@class-ops)))

(define (process-slot-option class ops k)
  (when (symbol? ops) (set! ops (cons ops '())))
  (let* ((specifier (car ops))
	 (slot-name (if (pair? specifier) (second specifier) specifier))
	 (new? (if (pair? specifier) #f #t))
	 (initform (undefined-value))
	 (slot-initargs '())
	 (defs '()))
    (for-each-key
      (lambda (key value)
	(case key
	  ((initform)
	    (set! initform `(lambda () ,value)))
	  ((reader)
	    (set! defs 
		  (cons `(define-reader ,value ,class ,(car ops)) 
			defs)))
	  ((writer)
	    (set! defs 
		  (cons `(define-writer ,value ,class ,(car ops))
			defs)))
	  ((accessor)
	    (set! defs 
		  (cons `(define-accessor ,value ,class ,(car ops))
		        defs)))
	  (else
	    (set! slot-initargs
		  (cons `(quote ,key) (cons value slot-initargs))))))
      (cdr ops))
    (k
      new?
      `(list 
        'name ',slot-name
	,@(if (undefined-value? initform) '() `('initfunction ,initform))
	,@slot-initargs)
      defs)))

(define (process-slot-options class ops k)
  (if (null? ops) (k '() '() '())
    (process-slot-option
      class
      (first ops)
      (lambda (new? forms defs)
	(process-slot-options
	  class
	  (rest ops)
	  (lambda (rest-forms rest-specs rest-defs)
	    (k (if new? (cons forms rest-forms) rest-forms)
	       (if new? rest-specs (cons forms rest-specs))
	       (append defs rest-defs))))))))

(define (process-class-options c ops k)
  (let ((defs '())
	(forms '())
	(meta (undefined-value)))
    (for-each-key
      (lambda (key value)
	(case key
	  ((initargs)
	    (set! forms (cons ''direct-initargs (cons `',value forms))))
	  ((predicate)
	    (set! defs (cons `(define-predicate ,value ,c) defs)))
	  ((constructor)
	    (set! defs (cons `(define-constructor ,value ,c) defs)))
	  ((metaclass)
	    (set! meta value))
	  (else
	    (set! forms (cons `',key (cons `,value forms))))))
      ops)
    (k (if (undefined-value? meta) '<class> meta) forms defs)))

(define-telos-syntax (define-allocated-class name supers slot-ops . class-ops)
  (let ((nice-name (strip-angle-brackets name)))
    (process-class-options name class-ops
      (lambda (meta class-forms class-defs)
	(process-slot-options name slot-ops
	  (lambda (slot-forms slot-specializers slot-defs)
	    `(begin
	       (let* ((@init-list@ (list
				    'name ',nice-name
				    'direct-superclasses (list ,@supers)
				    'direct-slot-descriptions
				      (list ,@slot-forms)
				    'inherited-slot-description-specializers
				      (list ,@slot-specializers)
				    ,@class-forms)))
		 (always:initialize-class ,name @init-list@))
	       ,@slot-defs
	       ,@class-defs
	       ',name)))))))

;; Not a good name! 

(define-telos-syntax (class-lambda supers slot-ops . class-ops)
  (let ((nice-name 'anonymous))
    (process-class-options 'nout class-ops
      (lambda (meta class-forms class-defs)
	(process-slot-options 'nout slot-ops
	  (lambda (slot-forms slot-specializers slot-defs)
	    (unless (and (null? slot-defs) (null? class-defs))
	      (telos-error "binding options are invalid in class-lambda"
			   supers slot-ops class-ops slot-defs class-defs))
	       `(make ,meta 
		      'name ',nice-name
		      'direct-superclasses (list ,@supers)
		      'direct-slot-descriptions
		        (list ,@slot-forms)
		      'inherited-slot-description-specializers
		        (list ,@slot-specializers)
			,@class-forms)))))))

(define-telos-syntax (define-reader name class slot)
  `(define ,name 
     (always:slot-description-reader
       (always:find-slot-description ,class ',slot))))

(define-telos-syntax (define-writer name class slot)
  `(define ,name 
     (always:slot-description-writer
       (always:find-slot-description ,class ',slot))))

(define-telos-syntax (define-accessor name class slot)
  `(begin
     (define-reader ,name ,class ,slot)
     (define-writer ,(string->symbol 
		       (string-append 
			 (symbol->string 'set-)
			 (symbol->string name) "!"))
                    ,class ,slot)))

(define-telos-syntax (define-constructor name-keys class)
  (let ((name (first name-keys))
	(keys (rest name-keys)))
    `(define ,name (compute-constructor ,class ',keys))))

(define-telos-syntax (define-predicate name wop)
  `(define-generic (,name (o <object>))
     method (((o <object>)) #f)
     method (((o ,wop)) #t)))

;; Boot syntax:

(define-telos-syntax (define-boot-class name breaker . slots)
  (define (boot-forms slots pos)
    (if (null? slots) '()
      (cons `(define-boot-accessors ,name ,breaker ,(first slots) ,pos)
	    (boot-forms (rest slots) (+ 1 pos)))))
  `(begin ,@(boot-forms slots 0)))

(define-telos-syntax (define-boot-readers class breaker slot posn)
  (let* ((nice (strip-angle-brackets class))
	 (real-read (symbol-append nice '- slot))
	 (hack-read (symbol-append '% real-read))
	 (safe-read (symbol-append 'safe: real-read)))
    `(begin
       (define (,hack-read o)
	 (primitive-ref o ,posn))
       (define (,safe-read o)
	 (if (direct-instance? o ,breaker)
	   (,hack-read o)
	   (,real-read o))))))

(define-telos-syntax (define-boot-writers class breaker slot posn)
  (let* ((nice (strip-angle-brackets class))
	 (real-write (symbol-append 'set- nice '- slot '!))
	 (hack-write (symbol-append '% real-write))
	 (safe-write (symbol-append 'safe: real-write)))
    `(begin
       (define (,hack-write o val)
	 (primitive-set! o ,posn val)
	 val)
       (define (,safe-write o val)
	 (if (direct-instance? o ,breaker)
	   (,hack-write o val)
	   (,real-write o val))))))

(define-telos-syntax (define-boot-accessors class breaker slot posn)
  `(begin
     (define-boot-readers ,class ,breaker ,slot ,posn)
     (define-boot-writers ,class ,breaker ,slot ,posn)))

(define-telos-syntax (define-safe-generic name-vars . options)
  `',(first name-vars))

(define-telos-syntax (define-safe-method name-vars . body)
  (let* ((name (first name-vars))
	 (safe (symbol-append 'safe: name))
	 (std (symbol-append 'std: name))
	 (vars (lambda-list-formals (rest name-vars)))
	 (classes (lambda-list-signature (rest name-vars)))
	 (arg1-name (first vars))
	 (arg1-class (first classes)))
    `(begin
       (define ,(cons std vars) ,@body)
       (define ,(cons safe vars)
	 (if (and (telos-booted?)
		  (not (direct-instance? ,arg1-name ,arg1-class)))
	     (,name ,@vars)
	     (,std ,@vars)))
       (define-method ,name-vars
	 (,std ,@vars)))))
	   

;; eof
