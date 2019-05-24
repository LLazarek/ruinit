#lang racket

(provide and/test
         and/test/message
         or/test
         if/test
         when/test
         unless/test
         not/test
         fail-when
         fail-unless
         for/and/test
         for/or/test)

(require "ruinit.rkt"
         (only-in rackunit
                  check-true
                  check-false
                  check-equal?))

(define-syntax (and/test stx)
  (syntax-parse stx
    [(_ t t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             (and/test t-rest ...)
             res)))]
    [(_)
     ;; lltodo: may want to do some kind of message aggregation here
     ;; instead of a fairly useless generic message.
     ;; Note sure how useful it would be.
     (syntax/loc stx
       (test-success "all ands passed"))]))
(define-syntax (and/test/message stx)
  (syntax-parse stx
    [(_ [t msg] t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             (and/test/message t-rest ...)
             (extend-test-message res
                                  (string-append msg " ")
                                  #:append? #f))))]
    [(_)
     ;; lltodo: may want to do some kind of message aggregation here
     ;; instead of a fairly useless generic message.
     ;; Note sure how useful it would be.
     (syntax/loc stx
       (test-success "and/test all tests passed"))]))
(define-syntax (or/test stx)
  (syntax-parse stx
    [(_ t t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             res
             (or/test t-rest ...))))]
    [(_)
     (syntax/loc stx
       (test-fail "or/test: all tests failed"))]))
(define-syntax-rule (if/test cond-t tt ff)
  (if (test-success? cond-t) tt ff))
(define-syntax-rule (when/test cond-t tt)
  (if/test cond-t tt (void)))
(define-syntax-rule (unless/test cond-t tt)
  (if/test cond-t (void) tt))

(define (invert-test-result t)
  (define msg (test-message t))
  (if (test-success? t)
      (test-fail msg)
      (test-success msg)))
;; lltodo: add option to not modify message
(define-syntax-rule (not/test t)
  (extend-test-message (invert-test-result t)
                       "not/test: "
                       #:append? #f))

;; These combinators allow short-circuiting failure based on another
;; test that automatically propogates the failure message
(define-syntax-rule (fail-when res)
  (let ([result res])
    (when/test result
      (fail (test-message result)))))
;; lltodo: add optional way to augment the failure message
(define-syntax-rule (fail-unless res)
  (let ([result res])
    (unless/test result
      (fail (test-message result)))))

(define-syntax-rule (succeed-when res)
  (let ([result res])
    (when/test result
      (succeed (test-message result)))))

;; lltodo: test/for/or, uses succeed-when (also a todo)
(define-test-syntax (for/and/test for-iter-expr
                                  e ...
                                  test)
  #`(for for-iter-expr
      e ...
      (fail-unless test
                   ;; lltodo: when add way to augment message with
                   ;; fail-when/unless, might be useful to add:
                   ;; "iteration bindings: ~a"
                   ;; `((id ,id) ...)
                   )))
(define-test-syntax (for/or/test for-iter-expr
                                 e ...
                                 test)
  #`(begin
      (for for-iter-expr
        e ...
        (succeed-when test))
      (fail "for/or/test: No iterations succeeded.")))

(module+ test
  (check-true (test-success? (and/test)))
  (check-true (test-fail? (and/test #f)))
  (check-true (test-fail? (and/test #f #t)))
  (check-true (test-success? (and/test #t #t)))
  ;; Test message propogation
  (check-equal? (test-message (and/test #t #t))
                "all ands passed")
  (check-equal? (test-message (and/test #t #f))
                #f)
  (check-equal? (test-message (and/test #t (test-fail "foobar")))
                "foobar")

  (check-true (test-success? (and/test/message)))
  (check-true (test-fail? (and/test/message [#f "blah"])))
  (check-equal? (test-message (and/test/message [#f "blah:"]))
                "blah: ")
  (check-true (test-fail? (and/test/message [#t "a"]
                                            [(test-fail "foo") "bar:"]
                                            [#t "b"])))
  (check-equal? (test-message (and/test/message [#t "a"]
                                                [(test-fail "foo") "bar:"]
                                                [#t "b"]))
                "bar: foo")

  (check-true (test-fail? (or/test)))
  (check-true (test-fail? (or/test #f)))
  (check-true (test-success? (or/test #f #t)))
  (check-true (test-success? (or/test #t #t)))
  ;; Test message propogation
  (check-equal? (test-message (or/test #t #t))
                #f)
  (check-equal? (test-message (or/test #t #f))
                #f)
  (check-equal? (test-message (or/test #f
                                       (test-success "foobar")
                                       (test-fail "baz")))
                "foobar")

  (check-equal? (if/test #t 1 2) 1)
  (check-equal? (if/test #f 1 2) 2)
  (check-equal? (if/test (test-success) 1 2) 1)
  (check-equal? (if/test (test-fail) 1 2) 2)

  (check-equal? (when/test #t 1) 1)
  (check-equal? (when/test #f 1) (void))
  (check-equal? (when/test (test-success) 1) 1)
  (check-equal? (when/test (test-fail) 1) (void))

  (check-equal? (unless/test #t 1) (void))
  (check-equal? (unless/test #f 1) 1)
  (check-equal? (unless/test (test-success) 1) (void))
  (check-equal? (unless/test (test-fail) 1) 1)

  (check-true (test-fail? (not/test #t)))
  (check-true (test-success? (not/test #f)))
  (check-true (test-fail? (not/test (test-success "foobar"))))
  (check-true (test-success? (not/test (test-fail "foobar"))))
  (check-equal? (test-message (not/test #t))
                "not/test: ")
  (check-equal? (test-message (not/test (test-success "foobar")))
                "not/test: foobar")
  (check-equal? (test-message (not/test (test-fail "foobar")))
                "not/test: foobar")

  (define-test (test-fail-unless x)
    (fail-unless x))
  (check-true (test-success? (test-fail-unless #t)))
  (check-true (test-fail? (test-fail-unless #f)))
  (check-true (test-success? (test-fail-unless
                              (test-success "abc went wrong"))))
  (check-true (test-fail? (test-fail-unless
                           (test-fail "abc went wrong"))))
  (check-equal? (test-message (test-fail-unless
                               (test-fail "abc went wrong")))
                "abc went wrong")

  (define-test (test-fail-when x)
    (fail-when x))
  (check-true (test-fail? (test-fail-when #t)))
  (check-true (test-success? (test-fail-when #f)))
  (check-true (test-success? (test-fail-when
                              (test-fail "abc went wrong"))))
  (check-true (test-fail? (test-fail-when
                           (test-success "abc went right"))))
  (check-equal? (test-message (test-fail-when
                               (test-success "abc went right")))
                "abc went right")


  (check-true (test-fail? (for/and/test ([x (in-list '(1 2 3))])
                                        (equal? x 1))))
  ;; Test extra exprs in body are ok
  (check-true (test-fail? (for/and/test ([x (in-list '(1 2 3))])
                                        (void)
                                        1 2 3
                                        (equal? x 1))))
  (check-true (test-success? (for/and/test ([x (in-list '(1 2 3))])
                                           (not (equal? x 4))))))
