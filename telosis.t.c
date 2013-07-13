;; Compiled loader.

(define-syntax (define-telos-syntax name-vars . body)
  `(define-syntax ,name-vars ,@body))

(define call/cc call-with-current-continuation)

(define native-substring substring)
(define (substring s i j) (native-substring s i (- j 1)))

(define simple-eval
  (let ((env (the-environment)))
    (lambda (exp) (eval exp env))))

(define scheme-error error)

(define *extension-files* '("gensym" "weak"))

(load "files.scm")

(for-each 
  (lambda (name)
    (load (string-append name ".so")))
 (append *extension-files* *telos-files*))
 
(start-telosis *telosis-version* "Compiled-T")

