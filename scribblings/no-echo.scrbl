#lang scribble/manual
@require[@for-label[no-echo
                    racket/base]]

@title{no-echo}
@author{djholtby}

@defmodule[no-echo]

This package allows for a echo-free readline when reading from terminal ports
On Windows this is limited to stdin.

@defproc[(read-line/password [#:stars? stars? any/c #f]
                            [#:in in input-port? (current-input-port)]
                            [#:out out output-port? (current-output-port)])
         string?]{
Reads a password from in without echoing keys pressed.  If @racket[stars?] is not @racket[#f] then a * will be
echoed to @racket[out] for each character of input read.  Otherwise the only output written to
@racket[out] is the newline that marks the end of passwor entry.

On Windows @racket[in] must be stdin.  On posix systems @racket[in] can be any terminal with
a file descriptor (it uses termios.h to control echo and canonical mode).  
}