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
  (char=? chr #\,))

(define (other-char? chr)
  (not (or
	(indenting-char? chr)
	(dedenting-char? chr)
	(comma? chr))))

(define (newline? chr)
  (char=? chr #\newline))

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
     (if (not (dedenting-char? (peek-char)))
	 (newline))
     (set! *indent-by* (+ *indent-by* 4))
     (if (not (dedenting-char? (peek-char)))
	 (dotimes *indent-by* (display " "))))
    ((dedenting-char? character)
     (newline)
     (dotimes (- *indent-by* 4) (display " "))
     (display character)
     (if (and (not (newline? (peek-char)))
	      (not (char=? #\, (peek-char)))
	      (not (dedenting-char? (peek-char))))
	 (newline))
     (set! *indent-by* (- *indent-by* 4)))
    ((comma? character)
     (display character)
     (newline)
     (dotimes *indent-by* (display " ")))
    ((other-char? character)
     (display character))))
