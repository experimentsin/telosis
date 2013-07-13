;; Portable gensym-ish code. Shouldn't cause problems in this lifetime
;; unless some other part of you application is using a hacked gensym 
;; of its own...

(define gensym
  (let ((count 9999))
    (lambda args
      (set! count (+ count 1))
      (string->symbol
        (string-append "G00" (number->string count 10))))))

;; eof
