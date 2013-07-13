;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: std-apps.scm,v 1.3 1992/08/10 12:38:57 kjp Exp kjp $
;;
;; $Log: std-apps.scm,v $
;; Revision 1.3  1992/08/10  12:38:57  kjp
;; Added set-slot-value!.
;;
;; Revision 1.2  1992/08/09  21:32:59  kjp
;; Feeler distribution version.
;;
;; Revision 1.1  1992/08/09  21:31:45  kjp
;; Initial revision
;;
;; Standard Applications.
;;
;;   Just a few useful routines for output and browsing.
;;
;

(define (class-slot-names c)
  (map slot-description-name (class-slot-descriptions c)))

(define (object-slot-names o)
  (class-slot-names (class-of o)))

(define-generic (show (o <object>)))

(define-method (show (o <object>))
  (display "#<")
  (show (class-name (class-of o)))
  (display ">")
  o)

(define-method (show (o <primitive-object>))
  (display o))

(define-method (show (o <pair>))
  (display "(")
  (let doit ((l o) (sep ""))
    (cond
      ((null? l) (display ")"))
      ((not (pair? l)) 
        (display " . ")
	(show l)
	(display ")"))
      (else
        (display sep)
	(show (car l))
	(doit (cdr l) " "))))
  o)

(define-method (show (o <class>))
  (display "#<")
  (display (class-name (class-of o)))
  (display ": ")
  (display (class-name o))
  (display ">")
  o)

(define-method (show (gf <generic>))
  (display "#<")
  (display (class-name (class-of gf)))
  (display ": ")
  (display (cons 
	  (generic-name gf) 
	  (map 
	     (lambda (ent)
	       (if (no-specifier? ent) '*
		 (class-name ent)))
	     (generic-signature gf))))
  (display " -> ")
  (display (class-name (generic-range gf)))
  (display ">")
  gf)

(define-method (show (gf <method>))
  (display "#<")
  (display (class-name (class-of gf)))
  (display ": ")
  (display (map 
	     (lambda (ent)
	       (if (no-specifier? ent) '*
		 (class-name ent)))
	     (method-signature gf)))
  (display ">")
  gf)

(define-method (show (sd <slot-description>))
  (display "#<")
  (display (class-name (class-of sd)))
  (display ": ")
  (display (slot-description-name sd))
  (display " ")
  (display (slot-description-position sd))
  (display ">")
  sd)

(define-generic (describe (o <object>)))

(define-method (describe (o <object>))
  (show o)
  (display " is of class ")
  (display (class-name (class-of o)))
  (newline)
  (for-each
    (lambda (sd)
      (display (slot-description-name sd))
      (display ": ")
      (show ((slot-description-reader sd) o))
      (newline))
    (class-slot-descriptions (class-of o)))
  o)

;; Stuff...

(define (display* . l)
  (for-each display l))

(define (start-telosis vers scheme)
  (let ((len (+ (string-length vers) (string-length scheme)))
	(herald "TelosiS: Version ")
	(copy "(C) 1992 Keith Playford"))
    (newline)
    (newline) 
    (display* herald "(" vers " " scheme ")")
    (indent-to (- 79 (+ len 
			(+ (string-length herald) 
			   (+ (string-length copy) 3)))))
    (display copy)
    (newline)
    (newline)
    (display 
      "Type (repl) to re-enter this evaluation loop after a scheme error.")
    (newline)
    (newline)
    (repl)))

(define telos-error-restart (lambda (v) (+ 'a 'a)))

(define (repl)
  (call/cc
    (lambda (return)
      (call/cc
        (lambda (restart)
	  (set! telos-error-restart (lambda () (restart #f)))
	  (let loop ()
	    (display "telosis> ")
	    (force-output (current-output-port))
	    (let ((form (read)))
	      (if (or (eof-object? form) (eq? form '!exit)) (return #t)
		  (let ((val (simple-eval form)))
		    (display "telosis< ")
		    (show val)
		    ;; (display " : ")
		    ;; (display (class-name (class-of val)))
		    (newline)
		    (newline))))
	    (loop))))
      (repl)))
  (set! telos-error-restart (lambda (v) (+ 'a 'a)))
  #t)

(define (telos-error mess . stuff)
  (newline)
  (display "TelosiS Error - ")
  (display mess)
  (display ".")
  (newline)
  (newline)
  (for-each 
    (lambda (o) 
      (display "Parameter: ")
      (show o) 
      (newline))
    stuff)
  (newline)
  (telos-error-restart))

(define (telos-warning mess . stuff)
  (newline)
  (display "TelosiS Warning - ")
  (display mess)
  (display ".")
  (newline)
  (newline)
  (for-each 
    (lambda (o) 
      (display "Parameter: ")
      (show o) 
      (newline))
    stuff)
  (newline))

(define (indent-to n)
  (unless (= n 0)
    (display " ")
    (indent-to (- n 1))))

(define (show-list-subtree cl indent)
  (unless (null? cl)
    (indent-to indent)
    (display (class-name (car cl)))
    (display " [")
    (display (class-name (class-of (car cl))))
    (display "]") 
    (newline)
    (show-list-subtree (class-direct-subclasses (car cl)) (+ indent 2))
    (show-list-subtree (cdr cl) indent))
  cl)

(define (show-subtree c) (show-list-subtree (list c) 0))

;; Slot access by name...

(define (slot-value o name)
  ((slot-description-reader (find-slot-description (class-of o) name)) o))

(define (set-slot-value! o name val)
  ((slot-description-writer (find-slot-description (class-of o) name)) o val))

;; eof

 