#lang racket

(provide test-begin
         module+test-begin
         display-test-results
         fail
         succeed
         define-test
         define-test-syntax
         define-simple-test
         test-result?
         test-success?
         test-fail?
         test-success
         test-fail
         test-message
         extend-test-message
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
(define/match (test-result/failure? t)
  [{(test-result #f _)} #t]
  [{_} #f])

(define (truthy->bool v) (if v #t #f))

(define/match (test-success? t)
  [{(test-result success? _)}
   (truthy->bool success?)]
  [{other-result}
   (truthy->bool other-result)])

(define test-fail? (negate test-success?))

(define (maybe-format msg fmt-args)
  (and msg (apply format msg fmt-args)))

(define (test-success [msg #f] . fmt-args)
  (test-result #t (maybe-format msg fmt-args)))
(define (test-fail [msg #f] . fmt-args)
  (test-result #f (maybe-format msg fmt-args)))

(define/match (test-message t)
  [{(test-result _ msg)} msg]
  [{_} #f])

(define/match (augment-message t augmenter)
  [{(test-result r msg) f}
   (test-result r (f msg))]
  [{other-result        f}
   (test-result other-result (f #f))])
(define (extend-test-message t fmt-str
                             #:append? [append? #t]
                             . fmt-args)
  (define (string-extend s1 s2)
    (apply string-append (if append?
                             (list s1 s2)
                             (list s2 s1))))
  (augment-message t
                   (match-lambda
                     [#f
                      ;; Just use the prepended msg
                      (maybe-format fmt-str fmt-args)]
                     [t-msg
                      (string-extend
                       t-msg
                       (or (maybe-format fmt-str fmt-args)
                           ""))])))


;; test-begin: Test ... -> void?
;; Optionally provide keyword `#:short-circuit` to cause tests
;; in this block to short circuit upon failure.
(define-syntax (test-begin stx)
  (syntax-parse stx
    ;; Discharge optional kws at top level of `test-begin`, so that
    ;; the case matching below doesn't need to deal with them
    [(_
      {~optional {~seq {~datum #:name} name}}
      {~optional {~and {~datum #:short-circuit} short-kw}}
      {~or* {~seq {~datum #:before} before-e:expr}
            {~seq {~datum #:after} after-e:expr}} ...+
      e:expr ...)
     ;; lltodo: ideally want to allow short-circuit interleaved
     ;; anywhere in befores and afters, but can't figure out how
     (define has-short-kw?  (attribute short-kw))
     #`(around
        (begin {~? before-e} ... (void))
        (test-begin/internal [{~? name #f} #,has-short-kw?] e ...)
        (begin {~? after-e} ... (void)))]
    [(_
      {~optional {~seq {~datum #:name} name}}
      {~optional {~and {~datum #:short-circuit} short-kw}}
      e:expr ...)
     (define has-short-kw?  (attribute short-kw))
     #`(test-begin/internal [{~? name #f} #,has-short-kw?] e ...)]))

(define-syntax (test-begin/internal stx)
  (syntax-parse stx
    [(_ [name short-circuit?])
     #'(void)]
    [(_ [name short-circuit?]
        ({~datum ignore} ignored-e ...)
        e:expr ...)
     (syntax/loc stx
       (begin
         ignored-e ...
         (test-begin/internal
          [name short-circuit?]
          e ...)))]
    [(_ [name short-circuit?]
        test:expr e ...)
     (define test-check
       (quasisyntax/loc stx
         (match test
           [(test-result #f msg)
            (register-test-failure! #'test msg (syntax->datum #'name))
            #f]
           [#f
            (register-test-failure! #'test #f (syntax->datum #'name))
            #f]
           [else
            (register-test-success!)
            #t])))
     (if (syntax->datum #'short-circuit?)
         (quasisyntax/loc stx
           ;; ll: cond allows internal definitions of later exprs
           ;; (could also wrap in a let, but I don't know if there's
           ;; any real difference)
           (cond [#,test-check
                  (test-begin/internal [name short-circuit?] e ...)]
                 [else
                  (void)]))
         (quasisyntax/loc stx
           (begin
             (void #,test-check)
             (test-begin/internal [name short-circuit?] e ...))))]))

(define-syntax-rule (module+test-begin body ...)
  (module+ test
    (test-begin
      body ...)))


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
  (define abbreviated (if (> len max)
                          (string-append (substring str 0 (- max 4)) "...")
                          str))
  (define quotes-in-abbreviated (regexp-match* #rx"\"" abbreviated))
  (if (and quotes-in-abbreviated
           (odd? (length quotes-in-abbreviated)))
      (string-append abbreviated "\"")
      abbreviated))

(define-syntax-rule (++! v)
  (set! v (add1 v)))


(define (failure-msg->string extras)
  (if extras
      (string-append "message:  " extras "\n")
      ""))

(define (test-group-name->string test-group-name)
  (if test-group-name
      (format "group:    ~a\n" test-group-name)
      ""))


(define test-count 0)
(define test-count/failed 0)

(define (register-test-failure! test-stx [msg #f] [test-group-name #f])
  (cond [(use-rackunit-backend)
         (rackunit/fail-test! test-stx msg test-group-name)]
        [else (printf
               #<<HERE
--------------- FAILURE ---------------
~alocation: ~a
test:     ~a
~a---------------------------------------

HERE
               (test-group-name->string test-group-name)
               (test-location->string (test-stx->location test-stx))
               (abbreviate-code (syntax->string #`(#,test-stx)))
               (failure-msg->string msg))
              (++! test-count)
              (++! test-count/failed)
              (test-log! #f)]))

(define (register-test-success!)
  (cond [(use-rackunit-backend)
         (rackunit/succeed-test!)]
        [else
         (++! test-count)
         (test-log! #t)]))

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

(define (rackunit/fail-test! test-stx [msg #f] [test-group-name #f])
  ((current-check-around)
   (λ _
     (with-check-info (['group (or test-group-name 'N/A)]
                       ['location (check-info-value
                                   (make-check-location
                                    (list
                                     (syntax-source test-stx)
                                     (syntax-line test-stx)
                                     (syntax-column test-stx)
                                     #f
                                     #f)))]
                       ['test (string->symbol
                               (abbreviate-code
                                (syntax->string #`(#,test-stx))))])
       (fail-check (failure-msg->string msg))))))

(define (rackunit/succeed-test!)
  ((current-check-around)
   (λ _ (void))))



(module+ test
  (define-syntax-rule (assert-output-match pat e)
    (let ([output (with-output-to-string (λ _ e))])
      (check-regexp-match pat output)))

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
                       (display-test-results))

  ;; Test before and after clauses
  (assert-output-match
   #rx"before!
test!
after1
after2
"
   (test-begin
     #:before (displayln 'before!)
     #:after (displayln 'after1)
     #:after (displayln 'after2)
     (displayln 'test!)))
  (assert-output-match
   #rx"test!
after1
after2
"
   (test-begin
     #:after (displayln 'after1)
     #:after (displayln 'after2)
     (displayln 'test!)))
  (assert-output-match
   #rx"before!
test!
"
   (test-begin
     #:before (displayln 'before!)
     (displayln 'test!)))
  ;; Test that after clauses run even when short-circuiting
  (assert-output-match
   #rx"test!
--------------- FAILURE ---------------
location: ruinit.rkt:[0-9]+:[0-9]+
test:     #f
---------------------------------------
after1
after2
"
   (test-begin
     #:short-circuit
     #:after (displayln 'after1)
     #:after (displayln 'after2)
     (displayln 'test!)
     #f
     (displayln 'should-never-see-me)))

  ;; Test that after clauses run even after crash inside the test-begin
  (assert-output-match
   #rx"test!
after1
after2
"
   (with-handlers ([exn:fail? (λ _ (void))])
     (test-begin
       #:short-circuit
       #:after (displayln 'after1)
       #:after (displayln 'after2)
       (displayln 'test!)
       (error 'crash!)
       (displayln 'should-never-see-me)))))





(require racket/stxparam)
(define-syntax-parameter fail
  (λ (stx)
    (raise-syntax-error
     'fail
     "can only be used within define-test* body"
     stx)))
(define-syntax-parameter succeed
  (λ (stx)
    (raise-syntax-error
     'succeed
     "can only be used within define-test* body"
     stx)))


(define-for-syntax (wrap-with-test-definer-env test-body-stx)
  (with-syntax ([body-stx test-body-stx])
    #'(let/ec escape
        (let* ([make-result-fn
                (λ (outcome)
                  (λ fmt-msg
                    (escape
                     (test-result outcome
                                  (apply format fmt-msg)))))]
               [fail-fn (make-result-fn #f)]
               [succeed-fn (make-result-fn #t)])
          #`(syntax-parameterize
                ([fail (make-rename-transformer #'fail-fn)]
                 [succeed (make-rename-transformer #'succeed-fn)])
              ;; Tests return whatever the body evals to
              ;; in order to allow them to act like simple predicates
              ;; by returning #t/#f to indicate outcome.
              #,body-stx)))))

(define-syntax (define-test stx)
  (syntax-parse stx
    [(_ (name:id . args) body:expr ...)
     #'(define (name . args)
         (let/ec escape
           (let* ([make-result-fn
                   (λ (outcome)
                     (λ fmt-msg
                       (escape
                        (test-result outcome
                                     (if (empty? fmt-msg)
                                         #f
                                         (maybe-format (first fmt-msg)
                                                       (rest fmt-msg)))))))]
                  [fail-fn (make-result-fn #f)]
                  [succeed-fn (make-result-fn #t)])
             (syntax-parameterize
                 ([fail (make-rename-transformer #'fail-fn)]
                  [succeed (make-rename-transformer #'succeed-fn)])
               ;; Tests return whatever the body evals to
               ;; in order to allow them to act like simple predicates
               ;; by returning #t/#f to indicate outcome.
               (begin body ...))))
         #;#,(wrap-with-test-definer-env #'(begin body ...)))]))

(define-syntax (define-test-syntax stx)
  (syntax-parse stx
    [(_ (name:id pat ...) pattern-directive ... body-e)
     #'(define-syntax (name s)
         (syntax-parse s
           [(_ pat ...)
            pattern-directive ...
            ;; Allow internal definitions
            (define body-stx (let () body-e))
            #`(let/ec escape
                (let* ([make-result-fn
                        (λ (outcome)
                          (λ fmt-msg
                            (escape
                             (test-result outcome
                                          (if (empty? fmt-msg)
                                              #f
                                              (maybe-format (first fmt-msg)
                                                            (rest fmt-msg)))))))]
                       [fail-fn (make-result-fn #f)]
                       [succeed-fn (make-result-fn #t)])
                  (syntax-parameterize
                      ([fail (make-rename-transformer #'fail-fn)]
                       [succeed (make-rename-transformer #'succeed-fn)])
                    ;; Tests return whatever the body evals to
                    ;; in order to allow them to act like simple predicates
                    ;; by returning #t/#f to indicate outcome.
                    #,body-stx)))
            #;#,(wrap-with-test-definer-env #'(begin body ...))]))]))


(define-syntax (define-simple-test stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...)
        (~optional (~seq (~datum #:success-message) succeed-msg:expr))
        (~optional (~seq (~datum #:fail-message) fail-msg:expr))
        body:expr ...)
     #'(define-test (name arg ...)
         (if (test-success? (let () body ...))
             (succeed (~? succeed-msg
                          (string-join (list (~a 'name)
                                             "succeeds with"
                                             (~v arg) ...))))
             (fail (~? fail-msg
                       (string-join (list (~a 'name)
                                          "fails with"
                                          (~v arg) ...))))))]))


(module+ test
  (define-syntax-rule (check-all e ...)
    (begin
      (check-true e)
      ...))

  ;; Check test-success? and test-fail?
  (check-all
   (test-success? #t)
   (test-success? (test-result #t ""))
   (not (test-success? #f))
   (not (test-success? (test-result #f "")))

   (not (test-fail? #t))
   (not (test-fail? (test-result #t "")))
   (test-fail? #f)
   (test-fail? (test-result #f "")))

  (define-test (foobar? a b)
    (unless (equal? a (+ b 1))
      (fail "~a and ~a don't foobar!" a b)))

  (check-all
    (test-result/failure? (foobar? 2 5))
    (equal? (test-result-message (foobar? 2 5))
            "2 and 5 don't foobar!")

    (test-success? (foobar? 2 1)))

  ;; Test succeed
  (define-test (foobar?/succeed a b)
    (if (equal? a (+ b 1))
        (succeed "They foobar!")
        (fail "~a and ~a don't foobar!" a b)))
  (check-all
    (test-result/failure? (foobar?/succeed 2 5))
    (equal? (test-result-message (foobar?/succeed 2 5))
            "2 and 5 don't foobar!")

    (test-success? (foobar?/succeed 2 1))
    (equal? (test-result-message (foobar?/succeed 2 1))
            "They foobar!"))


  (define-test-syntax (foobar?* a ... (~datum :)
                                [label b] ...)
    #:do [(define bazzle 2)]
    #:with [b2 ...] #'(b ...)
    #'(begin
        (unless (or a ...)
          (fail "At least one a has to succeed!"))
        (for ([el (in-list (list b2 ...))]
              [l  (in-list '(label ...))])
          (unless el
            (fail "Element ~a in b failed!" l)))))

  (check-all
    (test-result/failure? (foobar?* #f : [one #t] [two #t]))
    (equal? (test-result-message (foobar?* #f : [one #t] [two #t]))
            "At least one a has to succeed!")

    (test-success? (foobar?* #t : [one #t] [two #t]))

    (test-result/failure? (foobar?* #t #t : [one #f] [two #t]))
    (equal? (test-result-message (foobar?* #t #t : [one #f] [two #t]))
            "Element one in b failed!")

    (test-result/failure? (foobar?* #t #t : [one #t] [two #f]))
    (equal? (test-result-message (foobar?* #t #t : [one #t] [two #f]))
            "Element two in b failed!"))


  (define-simple-test (foobar?/simple a b c)
    #:fail-message (format "~a, ~a, ~a don't foobar!" a b c)
    (= a b (sub1 c)))
  (check-all
    (test-result/failure? (foobar?/simple 2 5 6))
    (equal? (test-result-message (foobar?/simple 2 5 6))
            "2, 5, 6 don't foobar!")

    (test-success? (foobar?/simple 2 2 3))
    (equal? (test-result-message (foobar?/simple 2 2 3))
            "foobar?/simple succeeds with 2 2 3"))

  (define-simple-test (foobar?/simple* a b c)
    (= a b (sub1 c)))
  (check-all
    (test-result/failure? (foobar?/simple* 2 5 (+ 3 3)))
    (equal? (test-result-message (foobar?/simple* 2 5 6))
            "foobar?/simple* fails with 2 5 6")

    (test-success? (foobar?/simple* 2 2 3))
    (equal? (test-result-message (foobar?/simple* 2 2 3))
            "foobar?/simple* succeeds with 2 2 3"))

  (define-simple-test (foobar?/simple** a b c)
    #:success-message "SUCCESS"
    #:fail-message (format "~a, ~a, ~a don't foobar!" a b c)
    (= a b (sub1 c)))
  (check-all
    (test-result/failure? (foobar?/simple** 2 5 (+ 3 3)))
    (equal? (test-result-message (foobar?/simple** 2 5 6))
            "2, 5, 6 don't foobar!")

    (test-success? (foobar?/simple** 2 2 3))
    (equal? (test-result-message (foobar?/simple** 2 2 3))
            "SUCCESS"))


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
     (error 'no-short-circuiting!)))

  ;; Test name on test-begin
  (assert-output-match
   #rx"--------------- FAILURE ---------------
group:    foobar-group
location: ruinit.rkt:[0-9]+:[0-9]+
test:     \\(equal\\? 1 0\\)
---------------------------------------
"
   (test-begin
     #:name foobar-group
     #:short-circuit
     (equal? 1 1)
     (ignore (define a 42))
     (equal? a a)
     (equal? 1 0)
     (equal? 2 3)
     (error 'no-short-circuiting!)))


  ;; Test test-success and test-fail outcome constructors
  (check-all
   (test-success? (test-success))
   (test-fail? (test-fail))
   (test-success? (test-success "~a" 5))
   (test-fail? (test-fail "~a" 5)))

  ;; Test test-message
  (check-all
   (false? (test-message #t))
   (false? (test-message #f))
   (false? (test-message (test-fail)))
   (equal? (test-message (test-fail "foobar ~v" 5))
           "foobar 5")
   (equal? (test-message (test-success "foobar ~v" 5))
           "foobar 5"))

  ;; Test augment-message
  (define (augmenter msg)
    (if msg
        (string-append msg ": blah")
        "replaced"))
  (check-all
   (test-fail? (augment-message (test-fail "foobar ~v" 5)
                                augmenter))
   (equal? (test-message (augment-message (test-fail "foobar ~v" 5)
                                          augmenter))
           "foobar 5: blah")

   (test-fail? (augment-message (test-fail)
                                augmenter))
   (equal? (test-message (augment-message (test-fail)
                                          augmenter))
           "replaced"))

  (check-all
   (test-fail? (extend-test-message (test-fail "foobar ~v" 5)
                                    ": blah ~v" 10))
   (equal? (test-message (extend-test-message (test-fail "foobar ~v" 5)
                                              ": blah ~v" 10))
           "foobar 5: blah 10")
   (test-fail? (extend-test-message (test-fail "foobar ~v" 5)
                                              "blah ~v: " 10
                                              #:append? #f))
   (equal? (test-message (extend-test-message (test-fail "foobar ~v" 5)
                                              "blah ~v: " 10
                                              #:append? #f))
           "blah 10: foobar 5")))
