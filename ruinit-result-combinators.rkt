#lang racket

(provide test-success?
         test-fail?
         test/and
         test/or
         test/if
         test/when
         test/unless)

(require "ruinit.rkt"
         (only-in rackunit
                  check-true
                  check-false
                  check-equal?))

(define/match (test-success? t)
  [{(test-result success? _)}
   success?]
  [{other-result} other-result])

(define test-fail? (negate test-success?))

(define-syntax-rule (test/and t ...)
  (test-result (and (test-success? t) ...) ""))
(define-syntax-rule (test/or t ...)
  (test-result (or (test-success? t) ...) ""))
(define-syntax-rule (test/if cond-t tt ff)
  (if (test-success? cond-t) tt ff))
(define-syntax-rule (test/when cond-t tt)
  (test/if cond-t tt (void)))
(define-syntax-rule (test/unless cond-t tt)
  (test/if cond-t (void) tt))

(module+ test
  (check-true (test-success? #t))
  (check-true (test-success? (test-result #t "")))
  (check-false (test-success? #f))
  (check-false (test-success? (test-result #f "")))

  (check-false (test-fail? #t))
  (check-false (test-fail? (test-result #t "")))
  (check-true (test-fail? #f))
  (check-true (test-fail? (test-result #f "")))

  (check-true (test-success? (test/and)))
  (check-true (test-fail? (test/and #f)))
  (check-true (test-fail? (test/and #f #t)))
  (check-true (test-success? (test/and #t #t)))

  (check-true (test-fail? (test/or)))
  (check-true (test-fail? (test/or #f)))
  (check-true (test-success? (test/or #f #t)))
  (check-true (test-success? (test/or #t #t)))

  (check-equal? (test/if #t 1 2) 1)
  (check-equal? (test/if #f 1 2) 2)
  (check-equal? (test/if (test-result #t "") 1 2) 1)
  (check-equal? (test/if (test-result #f "") 1 2) 2)

  (check-equal? (test/when #t 1) 1)
  (check-equal? (test/when #f 1) (void))
  (check-equal? (test/when (test-result #t "") 1) 1)
  (check-equal? (test/when (test-result #f "") 1) (void))

  (check-equal? (test/unless #t 1) (void))
  (check-equal? (test/unless #f 1) 1)
  (check-equal? (test/unless (test-result #t "") 1) (void))
  (check-equal? (test/unless (test-result #f "") 1) 1))
