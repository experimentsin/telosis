;; Stream-like stuff:

(define-class <eos-object> (<object>) 

  ((stream reader stream))
  
  initargs (stream)
  constructor (make-eos-object stream)
  predicate eos-object?)

(define-abstract-class <input-stream> (<object>) ())
(define-generic (input!     (s <input-stream>)))
(define-generic (peek-input (s <input-stream>)))

(define-class <output-stream> (<object>) ())
(define-generic (output! (o <output-stream>) value))

;; Filtered Streams:

(define-class <filtered-input-stream> (<input-stream>)

  ((stream reader filtered-input-stream-stream)
   (filter reader filtered-input-stream-filter))

  initargs (stream filter)
  constructor (make-filtered-input-stream stream filter))

(define-method (input! (s <filtered-input-stream>))
  ((filtered-input-stream-filter s) (filtered-input-stream-stream s)))

(define-class <filtered-output-stream> (<output-stream>)

  ((stream reader filtered-output-stream-stream)
   (filter reader filtered-output-stream-filter))

  initargs (stream filter)
  constructor (make-filtered-output-stream stream filter))

(define-method (output! (s <filtered-output-stream>) value)
  ((filtered-output-stream-filter s) 
    (filtered-output-stream-stream s)
    value))

;; File streams: