;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: primit.scm,v 1.1 1992/08/09 21:25:26 kjp Exp kjp $
;;
;; $Log: primit.scm,v $
;; Revision 1.1  1992/08/09  21:25:26  kjp
;; Initial revision
;;
;; Primitive object allocation.
;;
;;   Primitive interface to object allocation and slot access.
;;
;

;; Primitives:

(define *unbound-slot-value* (list 'slot-unbound)) ;; rebound later
(define (slot-unbound-value) *unbound-slot-value*)
(define (slot-unbound-value? o) (eq? *unbound-slot-value* o))

(define *primitive-tag* (list 'telos))
(define (primitive-object? o)
  (and (vector? o)
       (= (vector-length o) 2)
       (eq? (vector-ref o 0) *primitive-tag*)))

;
;; Gripe:
;;
;; So, here we are again tripping over Scheme's most appalling shortcoming -
;; no support for the definition of new disjoint types. Everytime I see 
;; an otherwise rational champion of Scheme post code such as that below 
;; and claim that Scheme doesn't need that facility, I cringe more 
;; violently. They are WRONG!!! You can almost, almost, *almost* do it 
;; but, in the end, whatever perverse disguise you've come up with for the
;; object, it will still answer true to one of the internal predicates and
;; submit to direct manipulation by the operations on that type. Even
;; common code is effected by this - in a type switch for example, I must
;; always make sure I test for new types before primitive types to avoid
;; error. This is unacceptable in a modern language.
;;
;; Fingers crossed that they stop hedging and do something about it.
;; 
;

;; This particular wacky rep was chosen because Telos objects tend to
;; be viciously circular - almost everything references everything else
;; through some chain - so it's beneficial to wrap these things up in
;; such a way that the standard Scheme output routines won't freak-out.

(define (make-primitive-object class slots)
  (vector
    *primitive-tag*
    (lambda (op index value)
      (case op
	((class-ref) class)
	((class-set) (set! class value) value)
	((ref) (vector-ref slots index))
	((set) (vector-set! slots index value) value)
	(else 'inconcievable!)))))

(define (primitive-allocate class size)
  (make-primitive-object class (make-vector size *unbound-slot-value*)))

(define (primitive-allocate-initialized class . values)
  (make-primitive-object class (apply vector values)))

(define (primitive-ref object index)
  ((vector-ref object 1) 'ref index #f))

(define (primitive-set! object index value)
  ((vector-ref object 1) 'set index value))

(define (primitive-class-ref object)
  ((vector-ref object 1) 'class-ref #f #f))

(define (primitive-class-set! object class)
  ((vector-ref object 1) 'class-set #f class))

;; eof
