#lang racket/base

(require ffi/unsafe/port ffi/unsafe ffi/unsafe/define racket/match)
(define-ffi-definer define-msvcrt (ffi-lib "msvcrt"))

(define-msvcrt _getwch (_fun -> _uint32))
(define-msvcrt _putwch (_fun _wchar -> _void))
(provide read-line/password)
  
(define scan-codes
  #hasheqv((0 . #hasheqv((0 . #\nul)
                         (59 . f1)
                         (60 . f2)
                         (61 . f3)
                         (62 . f4)
                         (63 . f5)
                         (64 . f6)
                         (65 . f7)
                         (66 . f8)
                         (67 . f9)
                         (68 . f10)
                         (71 . keypad-up+left)
                         (72 . keypad-up)
                         (73 . keypad-up+right)
                         (75 . keypad-left)
                         (77 . keypad-right)
                         (79 . keypad-down+left)
                         (80 . keypad-down)
                         (81 . keypad-down+right)
                         (82 . keypad-ins)
                         (83 . keypad-del)
                         (94 . ctrl+f1)
                         (95 . ctrl+f2)
                         (96 . ctrl+f3)
                         (97 . ctrl+f4)
                         (98 . ctrl+f5)
                         (99 . ctrl+f6)
                         (100 . ctrl+f7)
                         (101 . ctrl+f8)
                         (102 . ctrl+f9)
                         (103 . ctrl+f10)
                         (104 . alt+f1)
                         (105 . alt+f2)
                         (106 . alt+f3)
                         (107 . alt+f4)
                         (108 . alt+f5)
                         (109 . alt+f6)
                         (110 . alt+f7)
                         (111 . alt+f8)
                         (112 . alt+f9)
                         (113 . alt+f10)))
           (224 . #hasheqv((133 . f11)
                           (134 . f12)
                           (137 . ctrl+f11)
                           (138 . ctrl+f12)
                           (139 . alt+f12)
                           (140 . alt+f12)
                           (82 . ins)
                           (83 . del)
                           (146 . ctrl+ins)
                           (147 . ctrl+del)
                           (71 . home)
                           (73 . page-up)
                           (81 . page-down)
                           (79 . end)
                           (119 . ctrl+home)
                           (118 . ctrl+page-down)
                           (117 . ctrl+end)
                           (224 . #\à)))))
                           
                           
                         
                         

(define (win-read-char)
  (define num (_getwch))
  (case num
    [(65535 4) eof]
    [(0 224)
     (define scancode (_getwch))
     (hash-ref (hash-ref scan-codes num #hasheqv()) scancode (list 'unknown num scancode))]
    [else (integer->char num)]))


(define (read-line/password #:stars? [show-stars? #f]
                            #:in [in (current-input-port)]
                            #:out [out (current-output-port)])
  (if (and (terminal-port? in)
           (unsafe-port->file-descriptor in))
      (let loop ([buffer '()]
                 [count 0])
        (match (win-read-char)
          [(or #\newline #\return (? eof-object?))
           (displayln "")
           (list->string (reverse buffer))]
          [(or #\rubout #\backspace 'del 'keypad-del) (if (positive? count)
                                                          (begin
                                                            (when show-stars?
                                                              (display "\b \b" out)
                                                              (flush-output out))
                                                            (loop (cdr buffer) (sub1 count)))
                                                          (loop buffer count))]
          [(and c (or (? list?) (? symbol?) (? char-iso-control?)))
           (loop buffer count)]
          [(? char? c)
           (when show-stars? (display #\* out) (flush-output out))
           (loop (cons c buffer) (add1 count))]))
      (begin
        (displayln "**COULD NOT GET FD FROM INPUT PORT, PASSWORD ENTRY WILL ECHO**" out)
        (read-line))))
  