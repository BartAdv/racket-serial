#lang info

(define collection "serial")
(define deps '("base" "termios"))

(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"))
