#lang racket

(provide test/and
         test/and/message
         test/or
         test/if
         test/when
         test/unless
         test/not
         fail-when
         fail-unless
         test/for/and)

(require "ruinit.rkt"
         (only-in rackunit
                  check-true
                  check-false
                  check-equal?))

(define-syntax (test/and stx)
  (syntax-parse stx
    [(_ t t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             (test/and t-rest ...)
             res)))]
    [(_)
     ;; lltodo: may want to do some kind of message aggregation here
     ;; instead of a fairly useless generic message.
     ;; Note sure how useful it would be.
     (syntax/loc stx
       (test-success "all ands passed"))]))
(define-syntax (test/and/message stx)
  (syntax-parse stx
    [(_ [t msg] t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             (test/and/message t-rest ...)
             (extend-test-message res
                                  (string-append msg " ")
                                  #:append? #f))))]
    [(_)
     ;; lltodo: may want to do some kind of message aggregation here
     ;; instead of a fairly useless generic message.
     ;; Note sure how useful it would be.
     (syntax/loc stx
       (test-success "test/and all tests passed"))]))
(define-syntax (test/or stx)
  (syntax-parse stx
    [(_ t t-rest ...)
     (syntax/loc stx
       (let ([res t])
         (if (test-success? res)
             res
             (test/or t-rest ...))))]
    [(_)
     (syntax/loc stx
       (test-fail "test/or: all tests failed"))]))
(define-syntax-rule (test/if cond-t tt ff)
  (if (test-success? cond-t) tt ff))
(define-syntax-rule (test/when cond-t tt)
  (test/if cond-t tt (void)))
(define-syntax-rule (test/unless cond-t tt)
  (test/if cond-t (void) tt))

(define (invert-test-result t)
  (define msg (test-message t))
  (if (test-success? t)
      (test-fail msg)
      (test-success msg)))
;; lltodo: add option to not modify message
(define-syntax-rule (test/not t)
  (extend-test-message (invert-test-result t)
                       "test/not: "
                       #:append? #f))

;; These combinators allow short-circuiting failure based on another
;; test that automatically propogates the failure message
(define-syntax-rule (fail-when res)
  (let ([result res])
    (test/when result
      (fail (test-message result)))))
;; lltodo: add optional way to augment the failure message
(define-syntax-rule (fail-unless res)
  (let ([result res])
    (test/unless result
      (fail (test-message result)))))

;; lltodo: test/for/or, uses succeed-when (also a todo)
(define-test-syntax (test/for/and for-iter-expr
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

(module+ test
  (check-true (test-success? (test/and)))
  (check-true (test-fail? (test/and #f)))
  (check-true (test-fail? (test/and #f #t)))
  (check-true (test-success? (test/and #t #t)))
  ;; Test message propogation
  (check-equal? (test-message (test/and #t #t))
                "all ands passed")
  (check-equal? (test-message (test/and #t #f))
                #f)
  (check-equal? (test-message (test/and #t (test-fail "foobar")))
                "foobar")

  (check-true (test-success? (test/and/message)))
  (check-true (test-fail? (test/and/message [#f "blah"])))
  (check-equal? (test-message (test/and/message [#f "blah:"]))
                "blah: ")
  (check-true (test-fail? (test/and/message [#t "a"]
                                            [(test-fail "foo") "bar:"]
                                            [#t "b"])))
  (check-equal? (test-message (test/and/message [#t "a"]
                                                [(test-fail "foo") "bar:"]
                                                [#t "b"]))
                "bar: foo")

  (check-true (test-fail? (test/or)))
  (check-true (test-fail? (test/or #f)))
  (check-true (test-success? (test/or #f #t)))
  (check-true (test-success? (test/or #t #t)))
  ;; Test message propogation
  (check-equal? (test-message (test/or #t #t))
                #f)
  (check-equal? (test-message (test/or #t #f))
                #f)
  (check-equal? (test-message (test/or #f
                                       (test-success "foobar")
                                       (test-fail "baz")))
                "foobar")

  (check-equal? (test/if #t 1 2) 1)
  (check-equal? (test/if #f 1 2) 2)
  (check-equal? (test/if (test-success) 1 2) 1)
  (check-equal? (test/if (test-fail) 1 2) 2)

  (check-equal? (test/when #t 1) 1)
  (check-equal? (test/when #f 1) (void))
  (check-equal? (test/when (test-success) 1) 1)
  (check-equal? (test/when (test-fail) 1) (void))

  (check-equal? (test/unless #t 1) (void))
  (check-equal? (test/unless #f 1) 1)
  (check-equal? (test/unless (test-success) 1) (void))
  (check-equal? (test/unless (test-fail) 1) 1)

  (check-true (test-fail? (test/not #t)))
  (check-true (test-success? (test/not #f)))
  (check-true (test-fail? (test/not (test-success "foobar"))))
  (check-true (test-success? (test/not (test-fail "foobar"))))
  (check-equal? (test-message (test/not #t))
                "test/not: ")
  (check-equal? (test-message (test/not (test-success "foobar")))
                "test/not: foobar")
  (check-equal? (test-message (test/not (test-fail "foobar")))
                "test/not: foobar")

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


  (check-true (test-fail? (test/for/and ([x (in-list '(1 2 3))])
                                        (equal? x 1))))
  ;; Test extra exprs in body are ok
  (check-true (test-fail? (test/for/and ([x (in-list '(1 2 3))])
                                        (void)
                                        1 2 3
                                        (equal? x 1))))
  (check-true (test-success? (test/for/and ([x (in-list '(1 2 3))])
                                           (not (equal? x 4))))))
