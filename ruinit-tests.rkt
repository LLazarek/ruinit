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
         test-exn)

(require "ruinit.rkt"
         (for-syntax syntax/parse))

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
  equal?
  eqv?
  eq?
  =
  <
  >
  <=
  >=)

(define-test (test-within a b ε)
  (unless (<= (abs (- a b)) ε)
    (fail "~a and ~a are not within ~a of each other"
          a b ε)))

(define-test-syntax (test-match pat x)
  #'(unless (match x [pat #t] [else #f])
      (fail "~a fails to match pattern ~a"
            x 'pat)))

(define-test-syntax (test-exn exn-pred e)
  #'(with-handlers ([exn-pred (λ _ #t)])
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
    (equal? (test-message (test-equal? 1 0))
            "1 is not `equal?` to 0")
    (test-fail? (test-equal? 'a 'b))
    (equal? (test-message (test-equal? 'a 'b))
            "'a is not `equal?` to 'b")
    (test-fail? (test-equal? '(1 a 2 (3 #f))
                                       '(1 a 2 (3 #t))))
    (equal? (test-message (test-equal? '(1 a 2 (3 #f))
                                              '(1 a 2 (3 #t))))
            "'(1 a 2 (3 #f)) is not `equal?` to '(1 a 2 (3 #t))")
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

    (test-fail? (test-match (list a 2 a) '()))
    (test-fail? (test-match (list a 2 a) '(2 2 1)))
    (test-success? (test-match (list a 2 a) '(1 2 1)))

    (test-fail? (test-exn exn:fail? (+ 1 2)))
    (test-fail? (test-exn exn:fail? (string-append "a" "b")))
    (test-success? (test-exn exn:fail? (string-append "a" 1)))))
