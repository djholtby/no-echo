#lang racket/base

(require racket/port racket/system racket/string)

(provide post-installer)

(define (post-installer collect-dir dir)
  (unless (eq? (system-type 'os) 'windows)
    (let
        ([tmp (with-output-to-string
                (λ () (system "mktemp")))]
         [out (open-output-string)]
         [in (open-input-string ; gross way to do it...
              (string-append"#include <termios.h>\n#include <stddef.h>\n#include <poll.h>\n"
                            "tcflag_t tcf;"
                            "struct termios term;"
                            "char nccs[NCCS];"
                            "cc_t cc;"
                            "char echo[ECHO];"
                            "char icanon[ICANON];"
                            "char tcsanow[TCSANOW];"))])
      (when (parameterize ([current-output-port out]
                           [current-input-port in])
              (and (system (format "cc -x c -c -o '~a' -" tmp))
                   (system (format "nm '~a' --format=posix" tmp))
                   (system (format "rm '~a'" tmp))))
        (define sizes
          (make-hasheq
           (map
            (λ (los)
              (cons (string->symbol (car los))
                    (string->number (caddr los) 16)))
            (map string-split (string-split (get-output-string out) "\n")))))
        (define file (open-output-file (build-path dir "termios/sizes.rktd") #:exists 'replace))
        (write sizes file)
        (close-output-port file)))))
