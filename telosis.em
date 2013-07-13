;; TelosiS config and loader for FEEL.

;; Just (!> telosis) if this file's in your load path.
;; You need the most up to date Scheme module (post V0.75)

(defmodule telosis 

  (scheme 
   (only (make-table
	  table-ref
	  setter

	  with-handler 
	  push-handler
	  pop-handler
	  condition-message
	  condition-error-value) eulisp))

  ()

  ;; Syntax:

  (define define-telos-syntax
    (macro-lambda (name-vars . body)
      `(define ,(car name-vars) (macro-lambda ,(cdr name-vars) ,@body))))

  ;; Renaming:

  (define mapcar map) ;; For embarassing unhygienic macro-type reasons...

  ;; Portable extensions needed:

  (define *extension-files* '("gensym")) ;; "weak"

  (define make-weak-table make-table)
  (define weak-table-set! (setter table-ref))
  (define weak-table-ref 
    (lambda (tab key fail)
      (let ((res (table-ref tab key)))
	(if res res (fail)))))

  ;; Local extensions:

  (define simple-eval eval)

  ;; Load-up:

  (load "files.scm")

  (newline)
  (display "Loading TelosiS. Sorry.")
  (newline)
  (for-each load-telos-file (append *extension-files* *telos-files*))
  (display "hokay.")

  ;; Auto-start:

  (with-handler
    (lambda (c k)
      (telos-error "system error encountered"
		   (condition-message c)
		   (condition-error-value c)))
    (start-telosis *telosis-version* "FEEL"))

)

;; eof
