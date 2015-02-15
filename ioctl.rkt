#lang racket

(require ffi/unsafe
	 ffi/unsafe/define)

;; stuff that could be moved to some other collection

(define scheme_get_port_fd
  (get-ffi-obj 'scheme_get_port_fd #f (_fun _racket -> _intptr)))

(define _file-port/no-null
  (make-ctype _int
    (λ (x)
      (unless (port? x)
        (raise-argument-error '_file-port/no-null "file-stream-port" x))
      (scheme_get_port_fd x))
    (lambda (x) x)))

(define strerror
  (get-ffi-obj 'strerror #f (_fun _int -> _bytes)))

(define (check v who)
  (unless (zero? v)
    (let ([str (strerror (saved-errno))])
      (error who (bytes->string/locale str)))))

;; end of stuff that could be moved to some other collection

(provide get-ioctl-ffi)

(define-syntax (get-ioctl-ffi stx)
  (syntax-case stx ()
    [(_ type)
     #'(get-ffi-obj 'ioctl #f (_fun #:save-errno 'posix
				    _file-port/no-null
				    _ulong
				    (o : type)
				    -> (r : _int)
				    -> (when (check r 'ioctl) o)))]
    [(_)
     #'(get-ffi-obj 'ioctl #f (_fun #:save-errno 'posix
				    _file-port/no-null
				    _ulong
				    -> (r : _int)
				    -> (check r 'ioctl)))]))

