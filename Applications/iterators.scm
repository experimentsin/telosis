;; Abstract (should just be a specification):

(define-abstract-class <iteration-state> (<object>) ())

(define-generic (initial-state    (collection <object>)))
(define-generic (final-state      (collection <object>)))
(define-generic (current-element  (state <iteration-state>)))
(define-generic (completed-state? (state <iteration-state>)))
(define-generic (advance-state!   (state <iteration-state>)))

;; Useful generic implementation class:
;; (Makes hybrid use of objects+closures - bleargh!)

(define-class <generic-iteration-state> (<iteration-state>)

  ((position      accessor generic-iteration-position)
   (access        reader   generic-iteration-access)
   (next-position reader   generic-iteration-next-position)
   (done?         reader   generic-iteration-done?))

  initargs (position access next-position done?)
  constructor (make-generic-iteration-state position
					    access
					    next-position
					    done?)
  predicate generic-iteration-state?)

(define-method (current-element (s <generic-iteration-state>))
  ((generic-iteration-access s) (generic-iteration-position s)))

(define-method (completed-state? (s <generic-iteration-state>))
  ((generic-iteration-done? s) (generic-iteration-position s)))

(define-method (advance-state! (s <generic-iteration-state>))
  (let ((next-pos ((generic-iteration-next-position s)
		     (generic-iteration-position s))))
    (set-generic-iteration-position! s next-pos))
  s)

;; For built-in types...

;; Lists:

(define-method (initial-state (l <list>))
  (make-generic-iteration-state l first rest null?))

(define-method (final-state (l <list>))
  (make-generic-iteration-state (reverse l) first rest null?))

;; Array-like things:

(define (array-state array length ref forward?)
  (let ((len (length array)))
    (make-generic-iteration-state 
      0 
      (if forward? 
	(lambda (i) (ref array i))
	(lambda (i) (ref array (- (- len i) 1))))
      (lambda (i) (+ 1 i))
      (lambda (i) (>= i len)))))

;; Vectors:

(define-method (initial-state (v <vector>))
  (array-state v vector-length vector-ref #t))

(define-method (final-state (v <vector>))
  (array-state v vector-length vector-ref #f))

;; Strings:

(define-method (initial-state (s <string>))
  (array-state s string-length string-ref #t))

(define-method (final-state (s <string>))
  (array-state s string-length string-ref #f))

;; ---

;; Useful functions:

(define (map-collections->list f c . others)
  (let walk ((states (map initial-state (cons c others))))
    (if (any? completed-state? states) '()
      (let ((val (apply f (map current-element states))))
	(for-each advance-state! states)
	(cons val (walk states))))))

(define (for-each-in-collections f c . others)
  (let walk ((states (map initial-state (cons c others))))
    (unless (any? completed-state? states) 
      (apply f (map current-element states))
      (for-each advance-state! states)
      (walk states)))
  #t)

(define (foldl-states f acc state)
  (if (completed-state? state) acc
    (let ((val (current-element state)))
      (foldl-states f (f acc val) (advance-state! state)))))

(define (foldr-states f acc state)
  (if (completed-state? state) acc
    (let ((val (current-element state)))
      (f val (foldr-states f acc (advance-state! state))))))

(define (foldl-collection f acc c)
  (foldl-states f acc (initial-state c)))

(define (foldr-collection f acc c)
  (foldr-states f acc (initial-state c)))

;; e.g.

(map-collections->list list '(1 2 3) "abc" (vector 'x 'y 'z))
;; -> '((1 #\a x) (2 #\b y) (3 #\c z))

