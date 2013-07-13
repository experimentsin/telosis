;; Signal an error to the host Scheme.

(define (scheme-error mess . args)
  (newline)
  (display "Error - ")
  (display mess)
  (newline)
  (for-each
    (lambda (arg)
      (display "Parameter: ")
      (write arg)
      (newline))
    args)
  (newline)
  (+ 'a 'a))

;; eof
