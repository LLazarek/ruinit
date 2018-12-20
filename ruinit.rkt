#lang racket

(provide test-begin
         [struct-out test-result]
         test-result-failure?
         display-test-results
         fail
         define-test
         define-test-syntax
         define-simple-test
         max-code-display-length
         use-rackunit-backend
         ;; To support define-test-syntax
         (for-syntax (all-from-out syntax/parse)))

(require (for-syntax syntax/parse)
         syntax/to-string
         rackunit
         rackunit/log)

(define use-rackunit-backend (make-parameter #f))

;; A Test is either
;; - (any ... -> boolean?)
;; - (any ... -> test-result?)

(struct test-result (success? message))
(define test-result-failure? (negate test-result-success?))

;; test-begin: Test ... -> void?
;; Optionally provide keyword `#:short-circuit` to cause tests
;; in this block to short circuit upon failure.
(define-syntax (test-begin stx)
  (syntax-parse stx
    [(_ (~optional (~and (~datum #:short-circuit)
                         short-kw)))
     #'(void)]
    [(_ (~optional (~and (~datum #:short-circuit)
                         short-kw))
        ((~datum ignore) ignored-e ...)
        e:expr ...)
     (syntax/loc stx
       (begin
         ignored-e ...
         (test-begin (~? short-kw) e ...)))]
    [(_ (~optional (~and (~datum #:short-circuit)
                         short-kw))
        test:expr e ...)
     (define test-check
       (quasisyntax/loc stx
         (match test
           [(test-result #f msg)
            (register-test-failure! #'test msg)
            #f]
           [#f
            (register-test-failure! #'test)
            #f]
           [else
            (register-test-success!)
            #t])))
     (if (attribute short-kw)
         (quasisyntax/loc stx
           (cond [#,test-check
                  (test-begin short-kw e ...)]
                 [else (void)]))
         (quasisyntax/loc stx
           (begin
             (void #,test-check)
             (test-begin e ...))))]))



(struct test-location (path line column))

(define (test-stx->location test-stx)
  (test-location (syntax-source test-stx)
                 (syntax-line test-stx)
                 (syntax-column test-stx)))

(define (basename path)
  (if (symbol? path) ;; e.g. 'stdin
      path
      (let-values ([(_1 name _2) (split-path path)])
        name)))

(define/match (test-location->string loc)
  [{(test-location path line col)}
   (format "~a:~a:~a" (basename path) line col)])

(define max-code-display-length (make-parameter 70))
(define (abbreviate-code str)
  (define len (string-length str))
  (define max (max-code-display-length))
  (if (> len max)
      (string-append (substring str 0 (- max 3)) "...")
      str))

(define-syntax-rule (++! v)
  (set! v (add1 v)))


(define (failure-msg->string extras)
  (if extras
      (string-append "message:  " extras "\n")
      ""))


(define test-count 0)
(define test-count/failed 0)

(define (register-test-failure! test-stx [msg #f])
  (cond [(use-rackunit-backend)
         (rackunit/fail-test! test-stx msg)]
        [else (printf
               #<<HERE
--------------- FAILURE ---------------
location: ~a
test:     ~a
~a---------------------------------------

HERE
               (test-location->string (test-stx->location test-stx))
               (abbreviate-code (syntax->string #`(#,test-stx)))
               (failure-msg->string msg))
              (++! test-count)
              (++! test-count/failed)]))

(define (register-test-success!)
  (if (use-rackunit-backend)
      (rackunit/succeed-test!)
      (++! test-count)))

(define (display-test-results)
  (cond [(use-rackunit-backend)
         (void (test-log #:display? #t))]
        [else
         (newline)
         (displayln
          (match* (test-count test-count/failed)
            [(n n)
             (format "Every test (~a) failed." n)]
            [(n 0)
             (format "All ~a tests passed." n)]
            [(total failed)
             (format "~a of ~a tests passed." (- total failed) total)]))]))

(define (rackunit/fail-test! test-stx [msg #f])
  ((current-check-around)
   (λ _
     (with-check-info (['location (check-info-value
                                   (make-check-location
                                    (list
                                     (syntax-source test-stx)
                                     (syntax-line test-stx)
                                     (syntax-column test-stx)
                                     #f
                                     #f)))]
                       ['test (string->symbol (abbreviate-code (syntax->string #`(#,test-stx))))])
       (fail-check (failure-msg->string msg))))))

(define (rackunit/succeed-test!)
  ((current-check-around)
   (λ _ (void))))



(module+ test
  (define-syntax-rule (assert-output-match pat e)
    (let ([output (with-output-to-string (λ _ e))])
      (unless (regexp-match? pat output)
        (error (format "Output ~v does not match pattern ~v."
                       output pat)))))

  (assert-output-match "
Every test \\(0\\) failed.
"
                       (display-test-results))

  (test-begin
    (equal? 1 1))
  (assert-output-match "
All 1 tests passed.
"
                       (display-test-results))


  (assert-output-match
   #rx"--------------- FAILURE ---------------
location: ruinit.rkt:[0-9]+:[0-9]+
test:     \\(equal\\? 1 \\(\\+ 5 2\\)\\)
---------------------------------------
--------------- FAILURE ---------------
location: ruinit.rkt:[0-9]+:[0-9]+
test:     #f
---------------------------------------
"
   (test-begin
     (equal? 0 0)
     (equal? 2 2)
     (equal? 1 (+ 5 2))
     #f))
  (assert-output-match "
3 of 5 tests passed.
"
                       (display-test-results))

  (assert-output-match
   #rx"--------------- FAILURE ---------------
location: ruinit.rkt:[0-9]+:[0-9]+
test:     \\(test-result #f \"hahaha\"\\)
message:  hahaha
---------------------------------------
"
   (test-begin
     (equal? 1 1)
     (equal? 2 2)
     (equal? 1 1)
     (test-result #f "hahaha")))
  (assert-output-match "
6 of 9 tests passed.
"
                       (display-test-results)))




(require racket/stxparam)
(define-syntax-parameter fail
  (λ (stx)
    (raise-syntax-error
     'fail
     "can only be used within define-test* bodies")))


(define-syntax (define-test stx)
  (syntax-parse stx
    [(_ (name:id args:id ...) body:expr ...)
     #'(define (name args ...)
         (let/cc escape
           (let ([fail-fn (λ fmt-msg
                            (escape
                             (test-result #f
                                          (apply format fmt-msg))))])
             (syntax-parameterize
                 ([fail (make-rename-transformer #'fail-fn)])
               body ...)
             (test-result #t ""))))]))

(define-syntax (define-test-syntax stx)
  (syntax-parse stx
    [(_ (name:id pat:expr ...) body ...)
     #'(define-syntax (name s)
         (syntax-parse s
           [(_ pat ...)
            #'(let/cc escape
                (let ([fail-fn (λ fmt-msg
                                 (escape
                                  (test-result #f
                                               (apply format fmt-msg))))])
                  (syntax-parameterize
                      ([fail (make-rename-transformer #'fail-fn)])
                    body ...)
                  (test-result #t "")))]))]))


(define-syntax (define-simple-test stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        (~optional (~seq (~datum #:fail-message) msg:expr))
        body:expr ...)
     #'(define-test (name arg ...)
         (unless (begin body ...)
           (fail (~? msg
                     (string-join (list (~a 'name)
                                        "fails with"
                                        (~v arg) ...))))))]))


(module+ test
  (define-syntax-rule (assert-all e ...)
    (begin
      (unless e
        (error 'e))
      ...))

  (define-test (foobar? a b)
    (unless (equal? a (+ b 1))
      (fail "~a and ~a don't foobar!" a b)))

  (assert-all
    (test-result-failure? (foobar? 2 5))
    (equal? (test-result-message (foobar? 2 5))
            "2 and 5 don't foobar!")

    (not (test-result-failure? (foobar? 2 1)))
    (equal? (test-result-message (foobar? 2 1))
            ""))


  (define-test-syntax (foobar?* a ... (~datum :)
                                [label b] ...)
    (begin
      (unless (or a ...)
        (fail "At least one a has to succeed!"))
      (for ([el (in-list (list b ...))]
            [l  (in-list '(label ...))])
        (unless el
          (fail "Element ~a in b failed!" l)))))

  (assert-all
    (test-result-failure? (foobar?* #f : [one #t] [two #t]))
    (equal? (test-result-message (foobar?* #f : [one #t] [two #t]))
            "At least one a has to succeed!")

    (not (test-result-failure? (foobar?* #t : [one #t] [two #t])))

    (test-result-failure? (foobar?* #t #t : [one #f] [two #t]))
    (equal? (test-result-message (foobar?* #t #t : [one #f] [two #t]))
            "Element one in b failed!")

    (test-result-failure? (foobar?* #t #t : [one #t] [two #f]))
    (equal? (test-result-message (foobar?* #t #t : [one #t] [two #f]))
            "Element two in b failed!"))


  (define-simple-test (foobar?/simple a b c)
    #:fail-message (format "~a, ~a, ~a don't foobar!" a b c)
    (= a b (sub1 c)))
  (assert-all
    (test-result-failure? (foobar?/simple 2 5 6))
    (equal? (test-result-message (foobar?/simple 2 5 6))
            "2, 5, 6 don't foobar!")

    (not (test-result-failure? (foobar?/simple 2 2 3)))
    (equal? (test-result-message (foobar?/simple 2 2 3))
            ""))

  (define-simple-test (foobar?/simple* a b c)
    (= a b (sub1 c)))
  (assert-all
    (test-result-failure? (foobar?/simple* 2 5 (+ 3 3)))
    (equal? (test-result-message (foobar?/simple* 2 5 6))
            "foobar?/simple* fails with 2 5 6")

    (not (test-result-failure? (foobar?/simple* 2 2 3)))
    (equal? (test-result-message (foobar?/simple* 2 2 3))
            ""))


  ;; Test that test-begin ignore allows definitions
  (test-begin
    (equal? 1 1)
    (ignore (define a 42))
    (equal? a a))

  ;; Test short-circuiting version
  (assert-output-match
   #rx"--------------- FAILURE ---------------
location: ruinit.rkt:[0-9]+:[0-9]+
test:     \\(equal\\? 1 0\\)
---------------------------------------
"
   (test-begin
     #:short-circuit
     (equal? 1 1)
     (ignore (define a 42))
     (equal? a a)
     (equal? 1 0)
     (equal? 2 3)
     (error 'no-short-circuiting!))))
