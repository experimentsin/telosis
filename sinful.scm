;; Translated from EuLisp code, noddy expansion-passing macro expansion code.
;; Requires an eval.

  (define (map-improper f l)
    (if (not (pair? l)) l (cons (f (car l)) (map-improper f (cdr l)))))

  ;; Table utilities...

  (define (make-expander-table) 
    (list '()))

  (define (add-expander! tab key fn)
    (set-car! tab (cons (cons key fn) (car tab)))
    key)

  (define (masked-table-ref tab mask key)
    (and (not (memq key mask)) 
	 (let ((pair (assq key (car tab))))
	   (if pair (cdr pair) #f))))

  ;; Code...

  (define (make-expander tab mask)
    (define (new-expander form . new-mask)
      (set! new-mask (if (null? new-mask) '() (car new-mask)))
      (let ((whole-mask (append new-mask mask)))
	(macroexpand-form form tab whole-mask
			  (make-expander tab whole-mask))))
    new-expander)

  (define (macroexpand-form form tab mask expander)
    (if (not (pair? form)) form
      (let* ((op (car form))
	     (body (cdr form))
	     (expfn (masked-table-ref tab mask op)))
	(if expfn (apply expfn expander body)
	  (cons (expander op) (map-improper expander body))))))

  ;; Bootstrap...

  (define *expander-table* (make-expander-table))

  (define base-expander (make-expander *expander-table* '()))

  (add-expander! *expander-table* 'define-expander
    (lambda (expand name-vars . body)
      (expand
        `(add-expander! *expander-table* 
			',(car name-vars)
			(lambda ,(cdr name-vars) ,@body)))))

  (add-expander! *expander-table* 'define-syntax
    (lambda (expand name-vars . body)
      (expand 
        `(add-expander! *expander-table* 
			',(car name-vars)
			(lambda ,(cons '@expand@ (cdr name-vars))
			  (@expand@
			   (begin ,@body)))))))

  ;; Global:

  (define macroexpand (make-expander *expander-table* '()))

  (define (macroexpand-file in out)
    (let ((in (open-input-file in))
	  (out (open-output-file out)))
      (do ((form (read in) (read in)))
	  ((eof-object? form)
	    (close-input-port in)
	    (close-output-port out)
	    #t)
	(write (macroexpand form) out)
	(newline out))))

  ;; Simple, expanding eval:

  (define (expanding-eval form)
    (eval (macroexpand form)))

  (define (expanding-load file)
    (let ((in (open-input-file file)))
      (do ((form (read in) (read in)))
	  ((eof-object? form)
	    (close-input-port in)
	    #t)
	(expanding-eval form))))

(add-expander! *expander-table* (quote quote) (lambda (expand exp) (quasiquote (quote (unquote exp)))))
(add-expander! *expander-table* (quote quasiquote) (lambda (expand exp) (list (quote quasiquote) exp)))
(add-expander! *expander-table* (quote set!) (lambda (expand var exp) (quasiquote (set! (unquote var) (unquote (expand exp))))))
(add-expander! *expander-table* (quote lambda) (lambda (expand vars . body) (define (ll-vars ll) (cond ((null? ll) (quote ())) ((symbol? ll) (list ll)) (cons (car ll) (ll-vars (cdr ll))))) (quasiquote (lambda (unquote vars) (unquote (expand (cons (quote begin) body) (ll-vars vars)))))))
(quote (add-expander! *expander-table* (quote let) (lambda (expand binds . body) (quasiquote (let (unquote (map (lambda (bind) (cons (car bind) (list (expand (cadr bind))))) binds)) (unquote (expand (cons (quote begin) body))))))))
(add-expander! *expander-table* (quote let*) (lambda (expand binds . body) (quasiquote (let* (unquote (map (lambda (bind) (cons (car bind) (list (expand (cadr bind))))) binds)) (unquote (expand (cons (quote begin) body)))))))




