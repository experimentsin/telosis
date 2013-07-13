(define *telos-files* '("version"
			"sort"
			"utils"
			"macros"
			"primit"
			"boot"
			"mop"
			"std-apps"))

(define (telos-file->file-name tf) 
  (string-append tf ".scm"))

(define (load-telos-file tf)
  (load (telos-file->file-name tf)))

