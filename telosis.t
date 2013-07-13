;; TelosiS config and loader for interpreted T.

;; Make sure T is in Scheme mode by calling "(scheme-reset)" before 
;; attempting to load this file.

;; Syntax:

(define-syntax (define-telos-syntax name-vars . body)
  `(define-syntax ,name-vars ,@body))

;; Renaming:

(define call/cc call-with-current-continuation)

;; Portable extensions needed:

(define *extension-files* '("gensym" "weak"))

;; Local extensions:

(define native-substring substring) ;; T's has different semantics.
(define (substring s i j) (native-substring s i (- j 1)))

(define simple-eval
  (let ((env (the-environment)))
    (lambda (exp) (eval exp env))))

(define scheme-error error)

;; Load-up:

(load "files.scm")

(display "Loading TelosiS...")	
(for-each load-telos-file (append *extension-files* *telos-files*))
(display "hokay.")	

;; Compilation:

(define (compile-telosis)
  (for-each 
    compile-file
    (map telos-file->file-name (append *extension-files* *telos-files*))))

;; Auto-start:

(start-telosis *telosis-version* "Interpreted-T")

;; eof
