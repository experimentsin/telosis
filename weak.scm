;; Simple portable table implementation. Not weak at all (except in other
;; areas).

(define (make-weak-table pred)
  (cons pred '()))

(define (weak-table-set! tab key value)
  (let* ((alist (cdr tab))
	 (pred (car tab))
	 (pair (assq key alist)))
    (if pair (set-cdr! pair value)
      (set-cdr! tab (cons (cons key value) alist))))
  tab)

(define (weak-table-ref tab key fail)
  (let* ((alist (cdr tab))
	 (pred (car tab))
	 (pair (assq key alist)))
    (if pair (cdr pair) (fail))))

;; eof

