#lang racket/base

(require ffi/unsafe/port ffi/unsafe ffi/unsafe/define racket/match racket/system racket/port
         racket/string racket/list racket/runtime-path)
(define-ffi-definer define-stdlib (ffi-lib #f))
(provide read-line/password)

(define (size->uint_type int)
  (case int
    [(1) _uint8]
    [(2) _uint16]
    [(4) _uint32]
    [(8) _uint64]
    [else (error 'size->uint_type "invalid integer size ~v" int)]))

(define-runtime-path HERE ".")

(define sizes (with-input-from-file (build-path HERE "sizes.rktd") read))

(define-values
  (_tcflag_t _cc_t _NCCS _total_size_termios ECHO ICANON TCSANOW)
  (values (size->uint_type (hash-ref sizes 'tcf 4))
          (size->uint_type (hash-ref sizes 'cc 1))
          (/ (hash-ref sizes 'nccs 32)
             (hash-ref sizes 'cc 1))
          (hash-ref sizes 'term 60)
          (hash-ref sizes 'echo 8)
          (hash-ref sizes 'icanon 2)
          (hash-ref sizes 'tcsanow 1)))

;; minimal according to the spec
(define-cstruct _termios/minimal
  ([c_iflag _tcflag_t]
   [c_oflag _tcflag_t]
   [c_cflag _tcflag_t]
   [c_lflag _tcflag_t]
   [c_cc (_array _cc_t _NCCS)]))

;; cc reported size - minimal spec size = how much padding needed to avoid buffer overflows
(define padding-size (- _total_size_termios (ctype-sizeof _termios/minimal)))

(define-cstruct _termios
  ([c_iflag _tcflag_t]
   [c_oflag _tcflag_t]
   [c_cflag _tcflag_t]
   [c_lflag _tcflag_t]
   [c_cc (_array _cc_t _NCCS)]
   [optionals (_array/list _uint8 padding-size)]))

(define-cpointer-type _FILE* #:tag 'FILE)

(define-stdlib tcgetattr (_fun _int (t : (_ptr o _termios)) -> (r : _int) -> (values r t)))
(define-stdlib tcsetattr (_fun _int _int (_ptr i _termios) -> _int))
(define-stdlib getwchar (_fun -> _int32))
(define-stdlib getwc (_fun _FILE* -> _int32))
(define-stdlib fdopen (_fun _int _string -> _FILE*))
(define-stdlib fclose (_fun _FILE* -> _int))
(define-stdlib dup (_fun _int -> _int))

#|  if (echo) {
    settings.c_lflag |= ICANON;
    settings.c_lflag |= ECHO;
  } else {
    settings.c_lflag &= ~ICANON;
    settings.c_lflag &= ~ECHO;
  }
  tcsetattr(0, TCSANOW, &settings);
|#

(define (set-stdio-echo fd echo?)
  (define-values (rc settings) (tcgetattr fd))
  (define lflag (termios-c_lflag settings))
  (cond [echo?
         (set! lflag (bitwise-ior lflag ICANON))
         (set! lflag (bitwise-ior lflag ECHO))]
        [else
         (set! lflag (bitwise-and lflag (bitwise-not ICANON)))
         (set! lflag (bitwise-and lflag (bitwise-not ECHO)))])
  (set-termios-c_lflag! settings lflag)
  (tcsetattr fd TCSANOW settings))
         
;(set-stdio-echo #f)

(define (_read_wchar file)
  (define c (getwc file))
  (cond [(= c 27) ; escape 
         (define c (getwc file))
         (if (or (= c 91) (= c 79)) ; [ or O
             (let loop ()
               (define c (getwc file))
               (if (or (<= 65 c 90) ; cap
                       (<= 97 c 122); lc
                       (= c 126))
                   (_read_wchar file)
                   (loop)))
             c)]
        [else c]))
         
         

(define (chomp-escape-code)
  (let loop ()
    (let ([c (getwchar)])
      (when (or (= c (char->integer #\[))
                (= c (char->integer #\~))
                (= c (char->integer #\,))
                (= c (char->integer #\;))
                (<= (char->integer #\0) c (char->integer #\9)))
        (loop)))))

(define (read-line/password #:in [in (current-input-port)]
                            #:out [out (current-output-port)]
                            #:stars? [stars? #f])
  (flush-output)
  (define fd-original (unsafe-port->file-descriptor in))
  (if fd-original
      (let* ([fd (dup fd-original)]
             [file (fdopen fd "r")])
        (with-handlers ([exn? (λ (e)
                                (set-stdio-echo fd #t)
                                (fclose file)
                                (raise e))])
          (set-stdio-echo fd #f)
          (begin0
            (let loop ([buffer '()]
                       [count 0])
              (let ([c (_read_wchar file)])
                (cond [(negative? c) eof]
                      [(= 10 c) (list->string (reverse buffer))]
                      [(and (= 127 c) (zero? count))
                       (loop buffer count)]
                      [(= 127 c)
                       (when stars? (display "\b \b" out) (flush-output))
                       (loop (cdr buffer) (sub1 count))]
                      [(< c 32)
                       (loop buffer count)]
                      [else
                       (when stars? (display "*" out) (flush-output))
                       (loop (cons (integer->char c) buffer) (add1 count))])))
            (set-stdio-echo fd #t)
            (fclose file)
            (display #\newline))))
      (begin
        (displayln "**COULD NOT GET FD FROM INPUT PORT, PASSWORD ENTRY WILL ECHO**" out)
        (read-line))))

