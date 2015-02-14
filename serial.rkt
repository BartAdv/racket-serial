#lang racket

(require termios)
(require (for-syntax racket/syntax))

;; the setup is based on pyserial: https://github.com/makerbot/pyserial/blob/master/serial/serialposix.py
(define (setup port
	       baudrate
	       bytesize
	       stopbits
	       parity
	       xonxoff
	       rtscts
	       inter-char-timeout)
  (define t (tcgetattr port))
  ;; because setup consist of a sequence of conditional bit switching
  ;; let's define a macro to help us :>
  (define-syntax (set-flag stx)
    (syntax-case stx ()
      [(_ op field vs ...)
       (with-syntax ([setter (format-id #'field "set-TERMIOS-c_~a!" #'field)]
		     [getter (format-id #'field "TERMIOS-c_~a" #'field)])
	 #'(let [(ov (getter t))
		 (v (bitwise-ior vs ...))]
	     (if (eq? op 'on)
		 (setter t (bitwise-ior ov v))
		 (setter t (bitwise-and ov (bitwise-not v))))))]))
  (define-syntax-rule (flag field flag ...)
    (set-flag 'on field flag ...))
  (define-syntax-rule (unflag field flag ...)
    (set-flag 'off field flag ...))

  (define vmin (if inter-char-timeout 1 0))
  (define vtime (if inter-char-timeout (* 10 inter-char-timeout) 0))
  
  (flag cflag CLOCAL CREAD)
  (unflag lflag ICANON ECHO ECHOE ECHOK ECHONL (or ECHOCTL 0) (or ECHOKE 0) ISIG IEXTEN)
  (unflag oflag OPOST)
  (unflag iflag INLCR IGNCR ICRNL IGNBRK (or IUCLC 0) (or PARMRK 0))

  (unflag cflag CSIZE)
  (match bytesize
    [8 (flag cflag CS8)]
    [7 (flag cflag CS7)]
    [6 (flag cflag CS6)]
    [5 (flag cflag CS5)])

  (match stopbits
    ['one (unflag cflag CSTOPB)]
    ['one-point-five (flag cflag CSTOPB)]
    ['two (flag cflag CSTOPB)])

  (unflag iflag INPCK ISTRIP)
  (match parity
    ['none (unflag cflag PARENB PARODD)]
    ['even (begin
	     (unflag cflag PARODD)
	     (flag cflag PARENB))]
    ['odd (flag cflag PARENB PARODD)])

  (if IXANY 
      (if xonxoff
	  (flag iflag IXON IXOFF)
	  (unflag iflag IXON IXOFF IXANY))
      (if xonxoff
	  (flag iflag IXON IXOFF)
	  (unflag iflag IXON IXOFF)))
  (when CRTSCTS
    (if rtscts
	(flag cflag CRTSCTS)
	(unflag cflag CRTSCTS))) ; TODO: CNEW_RTSCTS?
  (tcsetattr port TCSANOW t)
  (TERMIOS-c_cflag t))
