;
;; TelosiS module (adapted from Eutopia)      Copyright (C) Keith Playford 1992
;

;
;; $Id: utils.scm,v 1.2 1992/08/09 21:30:09 kjp Exp $
;;
;; $Log: utils.scm,v $
;; Revision 1.2  1992/08/09  21:30:09  kjp
;; Feeler distribution version.
;;
;; Revision 1.1  1992/08/04  08:16:03  kjp
;; Initial revision
;;
;; Utilities.
;;
;;   Random useful functions, mainly list hacking. 
;;
;

;; Utils:

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)
(define (rest^2 l) (rest (rest l)))

(define (list-refs l il)
  (define (inner l pos next il)
    (if (= pos next) 
      (cons (first l)
	    (if (null? il) '() 
	      (inner (rest l) (+ 1 pos) (first il) (rest il))))
      (inner (rest l) (+ 1 pos) next il)))
  (if (null? il) '()
    (inner l 0 (first il) (rest il))))

(define (init-list-ref il key fail)
  (cond
    ((null? il) 
      (fail))
    ((eq? (first il) key)
      (second il))
    (else
      (init-list-ref (rest^2 il) key fail))))

(define (for-each-key fn kl)
  (if (null? kl) #f
    (begin
      (fn (car kl) (car (cdr kl)))
      (for-each-key fn (cdr (cdr kl))))))

(define *undefined-tag* (list 'undefined-value))
(define (undefined-value) *undefined-tag*)
(define (undefined-value? o) (eq? o *undefined-tag*))

(define (filter take? l)
  (if (null? l) '()
    (let ((a (first l)))
      (if (take? a) (cons a (filter take? (rest l)))
	  (filter take? (rest l))))))

(define (find-match take? l)
  (if (null? l) #f
    (let ((a (first l)))
      (if (take? a) a
	(find-match take? (rest l))))))

(define (all? ok? l)
  (if (null? l) #t (and (ok? (first l)) (all? ok? (rest l)))))

(define (any? ok? l)
  (if (null? l) #f (or (ok? (first l)) (any? ok? (rest l)))))

(define (foldl f acc l)
  (cond
    ((null? l) acc)
    (else      (foldl f (f acc (first l)) (rest l)))))

(define (weed l comp)
  (if (null? l) '()
    (let ((i (first l)))
      (cons i (filter (lambda (j) (not (comp i j))) (weed (rest l) comp))))))

(define (reverse-weed l comp)
  (reverse (weed (reverse l) comp)))
    
(define (map-appending f l . others)
  (if (null? others) (foldl append '() (map f l))
    (map-appending (lambda (x) x) (apply map (cons list (cons l others))))))

(define (position o l is?)
  (cond
    ((null? l) #f)
    ((is? o (first l)) 0)
    (else (+ 1 (position o (rest l) is?)))))

(define (positions-matching match? l)
  (let walk ((l l) (pos 0))
    (cond
      ((null? l) 
        '())
      ((match? (first l)) 
        (cons pos (walk (rest l) (+ 1 pos))))
      (else
        (walk (rest l) (+ 1 pos))))))

(define (take n l)
  (if (= n 0) '()
    (cons (first l) (take (- n 1) (rest l)))))

(define (drop n l)
  (if (= n 0) l
    (drop (- n 1) (rest l))))

(define (strip-angle-brackets sym)
  (let* ((name (symbol->string sym))
	 (len (string-length name)))
    (cond
      ((= len 0) sym)
      ((and (eqv? (string-ref name 0) #\<)
	    (eqv? (string-ref name (- len 1)) #\>))
        (string->symbol (substring name 1 (- len 1))))
      (else sym))))

(define (symbol-append . syms)
  (string->symbol
    (apply string-append (map symbol->string syms))))

(define (delete-matches match? l)
  (filter (lambda (elt) (not (match? elt))) l))

(define (delete item l equiv?)
  (delete-matches (lambda (elt) (equiv? item elt)) l))

(define (make-reader-name class slot)
  (let ((nice (strip-angle-brackets class)))
    (symbol-append nice '- slot)))

(define (make-writer-name class slot)
  (let ((nice (strip-angle-brackets class)))
    (symbol-append 'set- nice '- slot '!)))

;; eof
