#lang racket

(require termios)
(require (for-syntax racket/syntax))

;; the setup is based on pyserial: https://github.com/makerbot/pyserial/blob/master/serial/serialposix.py
(define (setup port)
  (define t (tcgetattr port))
  ;; because setup consist of a sequence of conditional bit switching
  ;; let's define a macro to help us
  (define-syntax (turn stx)
    (syntax-case stx ()
      [(_ op field vs ...)
       (with-syntax ([setter (format-id #'field "set-TERMIOS-c_~a!" #'field)]
		     [getter (format-id #'field "TERMIOS-c_~a" #'field)])
	 #'(let [(ov (getter t))
		 (v (apply bitwise-ior (list vs ...)))]
	     (if (eq? op 'on)
		 (setter t (bitwise-ior ov v))
		 (setter t (bitwise-and ov (bitwise-not v))))))]))
  (define (without n . flags)
    (bitwise-and n (bitwise-not (apply bitwise-ior flags))))
  (define cflag (bitwise-ior (TERMIOS-c_cflag t) CLOCAL CREAD))
  (define lflag (without (TERMIOS-c_lflag t)
			 ICANON
			 ECHO
			 ECHOE
			 ECHOK
			 ECHONL
			 (or ECHOCTL 0)
			 (or ECHOKE 0)
			 ISIG
			 IEXTEN))
  (define oflag (without (TERMIOS-c_oflag t) OPOST))
  (define iflag (without (TERMIOS-c_iflag t)
			 INLCR
			 IGNCR
			 ICRNL
			 IGNBRK
			 (or IUCLC 0)
			 (or PARMRK 0)))
  (tcsetattr port TCSANOW t)
  (TERMIOS-c_cflag t))
