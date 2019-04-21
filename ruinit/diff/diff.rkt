#lang racket

(provide diff-same-string
         diff-left-string
         diff-right-string
         dumb-diff-lines
         dumb-diff-lines/string)

(define diff-same-string (make-parameter "  "))
(define diff-left-string (make-parameter "< "))
(define diff-right-string (make-parameter "> "))

(define (print-diff diff-line-type line out)
  (displayln (string-append diff-line-type line) out))

;; input-port? input-port? output-port? -> void?
(define (dumb-diff-lines left right out)
  ;; Get around left and right having different numbers of lines
  ;; with this in-parallel
  (for ([left-line (in-sequences (in-lines left) (in-producer (const #f)))]
        [right-line (in-sequences (in-lines right) (in-producer (const #f)))]
        #:break (not (or left-line right-line)))
    (cond [(equal? left-line right-line)
           (print-diff (diff-same-string) left-line out)]
          [else
           (when left-line
             (print-diff (diff-left-string) left-line out))
           (when right-line
             (print-diff (diff-right-string) right-line out))])))

;; string? string? -> string?
(define (dumb-diff-lines/string left right)
  (define out (open-output-string))
  (dumb-diff-lines (open-input-string left)
              (open-input-string right)
              out)
  (get-output-string out))

(module+ test
  (require rackunit)
  (check-equal? (dumb-diff-lines/string "\n" "\n")
                (string-append (diff-same-string) "\n"))
  (check-equal? (dumb-diff-lines/string "haha\nb\n" "haha\nb\n")
                (string-append (diff-same-string) "haha\n"
                               (diff-same-string) "b\n"))
  (check-equal? (dumb-diff-lines/string "haha\nb\n" "haha\na\nb\nc\n")
                (string-append (diff-same-string) "haha\n"
                               (diff-left-string) "b\n"
                               (diff-right-string) "a\n"
                               (diff-right-string) "b\n"
                               (diff-right-string) "c\n")))
