#lang racket

(provide test-equal?
         test-eqv?
         test-eq?
         test-=
         test-<
         test->
         test-<=
         test->=
         test-within
         test-match
         test-exn
         equal?-diff-values
         equal?-pretty-print-values)

(require "ruinit.rkt"
         (for-syntax syntax/parse)
         racket/pretty
         "diff.rkt")

(define-syntax (define-binary-tests stx)
  (syntax-parse stx
    [(_ compare:id ...)
     (with-syntax ([(test-id ...)
                    (map add-test-prefix (syntax-e #'(compare ...)))])
       #'(begin
           (define-test (test-id a b)
             (if (compare a b)
                 (succeed "~v is `~a` to ~v" a 'compare b)
                 (fail "~v is not `~a` to ~v" a 'compare b)))
           ...))]))

(define-for-syntax (add-test-prefix id-stx)
  (define id-string (symbol->string (syntax->datum id-stx)))
  (define prefixed-id (string->symbol (string-append "test-"
                                                     id-string)))
  (datum->syntax id-stx prefixed-id))


(define-binary-tests
  eqv?
  eq?
  =
  <
  >
  <=
  >=)


(define equal?-diff-values (make-parameter #t))
(define equal?-pretty-print-values (make-parameter #t))

;; ll: custom equal? test to have nicer failure messages for complex values
(define-test (test-equal? a b)
  (define a/pretty (if (equal?-pretty-print-values)
                       (pretty-format a)
                       a))
  (define b/pretty (if (equal?-pretty-print-values)
                       (pretty-format b)
                       b))
  (define pretty-diff
    (if (and (equal?-diff-values)
             (not (or (base-value? a) (base-value? b))))
        (format "diff A B: A = <, B = >\n~a"
                (dumb-diff-lines/string a/pretty b/pretty))
        (format "A =\n~a\n\nB =\n~a" a/pretty b/pretty)))
  (if (equal? a b)
      (succeed "A is `equal?` to B:\nA = B =\n~a" a/pretty)
      (fail "A is not `equal?` to B:\n~a" pretty-diff)))

(define base-value? (disjoin number? symbol? string? char?))

(define-test (test-within a b ε)
  (unless (<= (abs (- a b)) ε)
    (fail "~a and ~a are not within ~a of each other"
          a b ε)))

(define-test-syntax (test-match x pat)
  #'(unless (match x [pat #t] [else #f])
      (fail "~a fails to match pattern ~a"
            x 'pat)))

(define-test-syntax (test-exn exn-pred e)
  #'(with-handlers ([exn-pred (λ _ #t)]
                    [exn? (λ (unrecognized)
                            (fail "Threw an unrecognized exception: ~v"
                                  unrecognized))])
      e
      (fail "Didn't throw exception recognized by ~a" 'exn-pred)))

(module+ test
  (test-begin
    (test-success? (test-equal? 1 1))
    (test-success? (test-equal? 'a 'a))
    (test-success? (test-equal? '(1 a 2 (3 #f))
                                           '(1 a 2 (3 #f))))
    (test-success? (test-equal? (mcons 1 2)
                                           (mcons 1 2)))
    (test-fail? (test-equal? 1 0))
    (equal? (test-message (test-= 1 0))
            "1 is not `=` to 0")
    (test-fail? (test-equal? 'a 'b))
    (equal? (test-message (test-equal? 'a 'b))
            "A is not `equal?` to B:
A =
'a

B =
'b")
    (test-fail? (test-equal? '(1 a 2 (3 #f))
                             '(1 a 2 (3 #t))))
    (equal? (test-message (test-equal? '(1 a 2 (3 #f))
                                              '(1 a 2 (3 #t))))
            "A is not `equal?` to B:
diff A B: A = <, B = >
< '(1 a 2 (3 #f))
> '(1 a 2 (3 #t))
")
    (test-fail? (test-equal? 2 2.0))

    ;; Taken straight from the docs
    (test-success? (test-eq? 'yes 'yes))
    (test-fail? (test-eq? 'yes 'no))
    (equal? (test-message (test-eq? 'yes 'no))
            "'yes is not `eq?` to 'no")
    (test-fail? (test-eq? (expt 2 100) (expt 2 100)))
    (test-fail? (test-eq? 2 2.0))
    (test-success? (let ([v (mcons 1 2)])
                     (test-eq? v v)))
    (test-fail? (test-eq? (mcons 1 2) (mcons 1 2)))
    (test-fail? (test-eq? (integer->char 955) (integer->char 955)))
    (test-fail? (test-eq? (make-string 3 #\z) (make-string 3 #\z)))

    ;; Taken straight from the docs
    (test-success? (test-eqv? 'yes 'yes))
    (test-fail? (test-eqv? 'yes 'no))
    (test-success? (test-eqv? (expt 2 100) (expt 2 100)))
    (test-fail? (test-eqv? 2 2.0))
    (test-success? (let ([v (mcons 1 2)])
                                (test-eqv? v v)))
    (test-fail? (test-eqv? (mcons 1 2) (mcons 1 2)))
    (test-success? (test-eqv? (integer->char 955)
                                     (integer->char 955)))
    (test-fail? (test-eqv? (make-string 3 #\z) (make-string 3 #\z)))


    (test-fail? (test-<= 2 1))
    (test-success? (test-<= 1 2))
    (test-success? (test-<= 1 1))

    (test-fail? (test-within 5 6 0.5))
    (test-fail? (test-within 0.8 1 0.1))
    (test-success? (test-within 5 6 1))
    (test-success? (test-within 5.1 5.1002 0.001))

    (test-fail? (test-match '() (list a 2 a)))
    (test-fail? (test-match '(2 2 1) (list a 2 a)))
    (test-success? (test-match '(1 2 1) (list a 2 a)))

    (test-fail? (test-exn exn:fail? (+ 1 2)))
    (test-fail? (test-exn exn:fail? (string-append "a" "b")))
    (string-prefix? (test-message
                     (test-exn exn:fail? (string-append "a" "b")))
                    "Didn't throw exception recognized by")
    (test-success? (test-exn exn:fail? (string-append "a" 1)))
    (test-fail? (test-exn exn:fail:contract:divide-by-zero?
                          (string-append "a" 1)))
    (string-prefix? (test-message
                     (test-exn exn:fail:contract:divide-by-zero?
                               (string-append "a" 1)))
                    "Threw an unrecognized exception:")))
