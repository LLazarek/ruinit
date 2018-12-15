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

(require "unit.rkt"
         (for-syntax syntax/parse))

(define-syntax (define-binary-tests stx)
  (syntax-parse stx
    [(_ compare:id ...)
     (with-syntax ([(test-id ...)
                    (map add-test-prefix (syntax-e #'(compare ...)))])
       #'(begin
           (define-test (test-id a b)
             (unless (compare a b)
               (fail "~v and ~v are not `~a`" a b 'compare)))
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
  (unless (match x [pat #t] [else #f])
    (fail "~a fails to match pattern ~a"
          x 'pat)))

(define-test-syntax (test-exn exn-pred e)
  (with-handlers ([exn-pred (λ _ #t)])
    e
    (fail "Didn't throw exception recognized by ~a" 'exn-pred)))

(module+ test
  (test-begin
    (not (test-result-failure? (test-equal? 1 1)))
    (not (test-result-failure? (test-equal? 'a 'a)))
    (not (test-result-failure? (test-equal? '(1 a 2 (3 #f))
                                            '(1 a 2 (3 #f)))))
    (not (test-result-failure? (test-equal? (mcons 1 2)
                                            (mcons 1 2))))
    (test-result-failure? (test-equal? 1 0))
    (equal? (test-result-message (test-equal? 1 0))
            "1 and 0 are not `equal?`")
    (test-result-failure? (test-equal? 'a 'b))
    (equal? (test-result-message (test-equal? 'a 'b))
            "'a and 'b are not `equal?`")
    (test-result-failure? (test-equal? '(1 a 2 (3 #f))
                                       '(1 a 2 (3 #t))))
    (equal? (test-result-message (test-equal? '(1 a 2 (3 #f))
                                              '(1 a 2 (3 #t))))
            "'(1 a 2 (3 #f)) and '(1 a 2 (3 #t)) are not `equal?`")
    (test-result-failure? (test-equal? 2 2.0))

    ;; Taken straight from the docs
    (not (test-result-failure? (test-eq? 'yes 'yes)))
    (test-result-failure? (test-eq? 'yes 'no))
    (equal? (test-result-message (test-eq? 'yes 'no))
            "'yes and 'no are not `eq?`")
    (test-result-failure? (test-eq? (expt 2 100) (expt 2 100)))
    (test-result-failure? (test-eq? 2 2.0))
    (not (test-result-failure? (let ([v (mcons 1 2)])
                                 (test-eq? v v))))
    (test-result-failure? (test-eq? (mcons 1 2) (mcons 1 2)))
    (test-result-failure? (test-eq? (integer->char 955) (integer->char 955)))
    (test-result-failure? (test-eq? (make-string 3 #\z) (make-string 3 #\z)))

    ;; Taken straight from the docs
    (not (test-result-failure? (test-eqv? 'yes 'yes)))
    (test-result-failure? (test-eqv? 'yes 'no))
    (not (test-result-failure? (test-eqv? (expt 2 100) (expt 2 100))))
    (test-result-failure? (test-eqv? 2 2.0))
    (not (test-result-failure? (let ([v (mcons 1 2)])
                                 (test-eqv? v v))))
    (test-result-failure? (test-eqv? (mcons 1 2) (mcons 1 2)))
    (not (test-result-failure? (test-eqv? (integer->char 955)
                                          (integer->char 955))))
    (test-result-failure? (test-eqv? (make-string 3 #\z) (make-string 3 #\z)))


    (test-result-failure? (test-<= 2 1))
    (not (test-result-failure? (test-<= 1 2)))
    (not (test-result-failure? (test-<= 1 1)))

    (test-result-failure? (test-within 5 6 0.5))
    (test-result-failure? (test-within 0.8 1 0.1))
    (not (test-result-failure? (test-within 5 6 1)))
    (not (test-result-failure? (test-within 5.1 5.1002 0.001)))

    (test-result-failure? (test-match (list a 2 a) '()))
    (test-result-failure? (test-match (list a 2 a) '(2 2 1)))
    (not (test-result-failure? (test-match (list a 2 a) '(1 2 1))))

    (test-result-failure? (test-exn exn:fail? (+ 1 2)))
    (test-result-failure? (test-exn exn:fail? (string-append "a" "b")))
    (not (test-result-failure? (test-exn exn:fail? (string-append "a" 1))))))
