(define-class <dylan-slotd> (<slot-description>)

  ((allocation accessor slot-description-allocation
	       initform 'instance))

)

(define-metaclass <dylan-class> (<class>)

  ((shared-values accessor class-shared-values))

)

(define-method (compute-defined-slot-description-class (c <dylan-class>)
						       spec)
  <dylan-slotd>)


		  