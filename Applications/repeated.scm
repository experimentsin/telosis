(define-class <repeated-> ()

  ((times accessor repeated-slot-description-times
	  initform 1))

  metaclass <abstract-class>)

(define-class <repeated-supporting-> () () <abstract-class>)

(define-method (compute-slot-reader (c <repeated-supporting->)
				    (sd <repeated->))
  (generic-lambda (object index)))

(define-method (compute-slot-writer (c <repeated-supporting->)
				    (sd <repeated->))
  (generic-lambda (object index value)))