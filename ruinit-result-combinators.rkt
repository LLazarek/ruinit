#lang racket

(provide test/and
         test/and/message
         test/or
         test/if
         test/when
         test/unless
         test/not)

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
             (test-fail "~a: ~a" msg (test-message res)))))]
    [(_)
     ;; lltodo: may want to do some kind of message aggregation here
     ;; instead of a fairly useless generic message.
     ;; Note sure how useful it would be.
     (syntax/loc stx
       (test-success "all ands passed"))]))
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
       (test-fail "all ors failed"))]))
(define-syntax-rule (test/if cond-t tt ff)
  (if (test-success? cond-t) tt ff))
(define-syntax-rule (test/when cond-t tt)
  (test/if cond-t tt (void)))
(define-syntax-rule (test/unless cond-t tt)
  (test/if cond-t (void) tt))
(define-syntax-rule (test/not t)
  (let* ([res t]
         [msg (test-message res)]
         [msg+not (and msg (string-append "test/not: " msg))])
    (if (test-success? res)
        (test-fail msg+not)
        (test-success msg+not))))

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
  (check-equal? (test-message (test/and/message [#f "blah"]))
                "blah: #f")
  (check-true (test-fail? (test/and/message [#t "a"]
                                            [(test-fail "foo") "bar"]
                                            [#t "b"])))
  (check-equal? (test-message (test/and/message [#t "a"]
                                                [(test-fail "foo") "bar"]
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
                #f)
  (check-equal? (test-message (test/not (test-success "foobar")))
                "test/not: foobar")
  (check-equal? (test-message (test/not (test-fail "foobar")))
                "test/not: foobar"))
