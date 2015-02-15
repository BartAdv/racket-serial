#lang racket

(require termios
	 (only-in ffi/unsafe _ptr _int))
(require (for-syntax racket/syntax))

(require "ioctl.rkt")

(define TIOCMBIC #x5417)
(define TIOCM_DTR #x002)
(define FIONREAD #x541B)

(define TIOCSBRK #x5427)
(define TIOCCBRK #x5428)

(define baudrate-constants
  (hash 0       B0
	50      B50     
	75      B75     
	110     B110    
	134     B134    
	150     B150    
	200     B200    
	300     B300    
	600     B600    
	1200    B1200   
	1800    B1800   
	2400    B2400   
	4800    B4800   
	9600    B9600   
	19200   B19200  
	38400   B38400  
	57600   B57600  
	115200  B115200 
	230400  B230400 
	460800  B460800 
	500000  B500000 
	576000  B576000 
	921600  B921600 
	1000000 B1000000
	1152000 B1152000
	1500000 B1500000
	2000000 B2000000
	2500000 B2500000
	3000000 B3000000
	3500000 B3500000
	4000000 B4000000))

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

  (define cc (TERMIOS-c_cc t))
  
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
  
  (vector-set! cc VMIN vmin)
  (vector-set! cc VTIME vtime)
  (set-TERMIOS-c_cc! t cc)
  
  (set-TERMIOS-c_ispeed! t (hash-ref baudrate-constants baudrate))
  (set-TERMIOS-c_ospeed! t (hash-ref baudrate-constants baudrate))
  
  (tcsetattr port TCSANOW t)
  t)

(define baudrate? (curry hash-has-key? baudrate-constants))

(provide (contract-out
	  [serial-open (-> path-string?
			   #:baudrate baudrate?
			   #:bytesize (or/c 8 7 6 5)
			   #:stopbits (or/c 'one 'one-point-five 'two)
			   #:parity (or/c 'none 'even 'odd)
			   #:xonxoff boolean?
			   #:rtscts boolean?
			   #:inter-char-timeout integer?
			   (values input-port? output-port?))]))
(define (serial-open path
		     #:baudrate [baudrate 9600]
		     #:bytesize [bytesize 8]
		     #:stopbits [stopbits 'one]
		     #:parity [parity 'even]
		     #:xonxoff [xonxoff #f]
		     #:rtscts [rtscts #f]
		     #:inter-char-timeout [timeout 3])
  (let-values ([(in out) (open-input-output-file path #:exists 'append)])
    ;; input/output ports share file descriptor, so it doesn't matter
    ;; on which one the setup is called
    (setup in baudrate bytesize stopbits parity xonxoff rtscts timeout)
    (values in out)))

(define (serial-close in out)
  (close-input-port in)
  (close-output-port out))

(define (send-break port duration)
  (tcsendbreak port duration))

(define (flip f) (lambda (x y) (f y x)))

(define set-break
  (let [(f (get-ioctl-ffi))]
    (λ (port [set? #t])
     (f port (if set? TIOCSBRK TIOCCBRK)))))

(define in-waiting
  (let [(f (get-ioctl-ffi (_ptr o _int)))]
    (curry (flip f) FIONREAD)))
