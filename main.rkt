#lang racket/base

(require racket/runtime-path)
(provide read-line/password)

(define-runtime-path MSVCRT "msvcrt/msvcrt.rkt")
(define-runtime-path TERMIOS "termios/termios.rkt")

(define read-line/password
  (dynamic-require 
   (if (eq? (system-type 'os) 'windows) MSVCRT TERMIOS)
   'read-line/password))


(module+ main
  (read-line/password #:stars? #t))