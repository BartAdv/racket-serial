#lang racket/base

(require make/setup-extension)

(provide pre-installer)

(define (pre-installer collections-top-path racket-serial-path)
  (pre-install racket-serial-path
	     (build-path racket-serial-path "private")
	     "defines.c"
	     "."
	     '()
	     '()
	     '()
	     '()
	     '()
	     '()
	     (lambda (thunk) (thunk))
	     #t))
