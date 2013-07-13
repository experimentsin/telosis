(define-class <constrained-generic> (<generic>)

  ((pre-condition  reader generic-pre-condition
		   initform #f)
   (post-condition reader generic-post-condition
		   initform #f))

  initargs (pre-condition post-condition)
  metaclass <callable-class>)

(define-method (compute-discriminating-function (gf <constrained-generic>)
						sig
						lookup
						methods)
  (let ((std (call-next-method))
	(pre (generic-pre-condition gf))
	(post (generic-post-condition gf)))
    ;; Preserve tail-calls when we can...
    (cond
      ((not (or pre post)) 
        std)
      ((and pre post)
        (lambda args
	  (unless (apply pre args)
	    (telos-error "pre-condition not met" gf args))
	  (let ((res (apply std args)))
	    (unless (apply post args)
	      (telos-error "post-condition not met" gf args))
	    res)))
      (pre
        (lambda args
	  (unless (apply pre args)
	    (telos-error "pre-condition not met" gf args))
	  (apply std args)))
      (post
        (lambda args
	  (let ((res (apply std args)))
	    (unless (apply post args)
	      (telos-error "post-condition not met" gf args))
	    res))))))

;; e.g.

;; Type:

(define-abstract-class <stack> (<object>) ())

(define-generic (empty-stack? (s <stack>)))

(define-generic (push! val (s <stack>))
  class <constrained-generic>
  post-condition (lambda (val s) (not (empty-stack? s))))

(define-generic (pop! (s <stack>))
  class <constrained-generic>
  pre-condition (lambda (s) (not (empty-stack? s))))

;; Imp:

(define-class <working-stack> (<stack>)

  ((values accessor working-stack-values
	   initform '()))

  constructor (make-working-stack))

(define-method (empty-stack? (s <working-stack>))
  (null? (working-stack-values s)))

(define-method (push! val (s <working-stack>))
  (set-working-stack-values! 
    s
    (cons val (working-stack-values s)))
  val)

(define-method (pop! (s <working-stack>))
  (let ((values (working-stack-values s)))
    (set-working-stack-values! s (rest values))
    (first values)))

;; e.g.

(define s (make-working-stack))
(push! 12 s)
(pop! s)
;; => 12
;; (pop! s)
;; would => constraint not met error.

