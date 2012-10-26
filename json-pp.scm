#!/usr/local/bin/scsh \
-e main -s
!#

(define *indent-by* 0)

(define (indenting-char? chr)
    (cond ((char=? chr #\{) #t)
          ((char=? chr #\[) #t)
          (else #f)))

(define (dedenting-char? chr)
    (cond ((char=? chr #\}) #t)
          ((char=? chr #\]) #t)
          (else #f)))

(define (comma? chr)
    (if (char=? chr #\,)
        #t
        #f))

(define (other-char? chr)
  (not (or
	(indenting-char? chr)
	(dedenting-char? chr)
	(comma? chr))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes count body ...)
     (let loop ((counter count))
       (if (> counter 0)
           (begin
             body ...
             (loop (- counter 1))))))))

(define (main argv)
  (awk (read-char) (character) ()
    ((indenting-char? character)
     (display character)
     (newline)
     (set! *indent-by* (+ *indent-by* 4))
     (dotimes *indent-by* (display " ")))
    ((dedenting-char? character)
     (newline)
     (dotimes (- *indent-by* 4) (display " "))
     (display character)
     (newline)
     (set! *indent-by* (- *indent-by* 4))
     (dotimes *indent-by* (display " ")))
    ((comma? character)
     (display character)
     (newline)
     (dotimes *indent-by* (display " ")))
    ((other-char? character)
     (display character))))
