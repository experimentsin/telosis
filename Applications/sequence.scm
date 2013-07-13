(define-class <sequence> (<object>) () metaclass <abstract-class>)

;; Primary:

(define-generic (sequence-length (s <sequence>)))
(define-generic (sequence-nth (s <sequence>) n))
(define-generic (set-sequence-nth! (s <sequence>) n value))

;; Derived:

(define-generic (map-sequence fn (s <sequence>)))

(define-method (map-sequence fn (s <sequence>)))


  
  