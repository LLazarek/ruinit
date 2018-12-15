#lang racket

;; TODO:
;; - [ ] Handle exceptions? -- This could easily be a simple user test macro.

(provide test-begin
         [struct-out test-result]
         display-test-results
         fail
         define-test
         define-test-syntax)

(require (for-syntax syntax/parse)
         syntax/to-string)

;; A Test is either
;; - (any ... -> boolean?)
;; - (any ... -> test-result?)

(struct test-result (failure? message))

;; run-tests: Test ... -> void?
(define-syntax (test-begin stx)
  (syntax-parse stx
    [(_) #'(void)]
    [(_ ((~datum ignore) ignored-e ...)
        e:expr ...)
     (syntax/loc stx
       (begin
         ignored-e ...
         (test-begin e ...)))]
    [(_ test:expr e ...)
     (quasisyntax/loc stx
       (begin
         (match test
           [(test-result #t msg)
            (register-test-failure! #'test msg)]
           [#f
            (register-test-failure! #'test)]
           [else
            (register-test-success!)])
         (test-begin e ...)))]))

(struct test-location (path line column))

(define (test-stx->location test-stx)
  (test-location (syntax-source test-stx)
                 (syntax-line test-stx)
                 (syntax-column test-stx)))

(define (basename path)
  (define-values (_1 name _2) (split-path path))
  name)

(define/match (test-location->string loc)
  [{(test-location path line col)}
   (format "~a:~a:~a" (basename path) line col)])

(define-syntax-rule (++! v)
  (set! v (add1 v)))


(define (failure-extras->string extras)
  (if extras
      (string-append "message:  " extras "\n")
      ""))


(define test-count 0)
(define test-count/failed 0)

(define (register-test-failure! test-stx [msg #f])
  (printf
   #<<HERE
--------------- FAILURE ---------------
location: ~a
test:     ~a
~a---------------------------------------

HERE
   (test-location->string (test-stx->location test-stx))
   (syntax->string #`(#,test-stx))
   (failure-extras->string msg))
  (++! test-count)
  (++! test-count/failed))

(define (register-test-success!)
  (++! test-count))

(define (display-test-results)
  (newline)
  (displayln
   (match* (test-count test-count/failed)
     [(n n)
      (format "Every test (~a) failed." n)]
     [(n 0)
      (format "All ~a tests passed." n)]
     [(total failed)
      (format "~a of ~a tests passed." (- total failed) total)])))


(module+ test
  (test-begin
    (equal? 0 0)
    (equal? 2 2)
    (equal? 1 (+ 5 2))
    #f)

  (test-begin
    (equal? 1 1)
    (equal? 2 2)
    (equal? 1 1)
    (test-result #t "hahaha"))

  (display-test-results))




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
                             (test-result #t
                                          (apply format fmt-msg))))])
             (syntax-parameterize
                 ([fail (make-rename-transformer #'fail-fn)])
               body ...)
             (test-result #f ""))))]))

(define-syntax (define-test-syntax stx)
  (syntax-parse stx
    [(_ (name:id pat:expr ...) body ...)
     #'(define-syntax (name s)
         (syntax-parse s
           [(_ pat ...)
            #'(let/cc escape
                (let ([fail-fn (λ fmt-msg
                                 (escape
                                  (test-result #t
                                               (apply format fmt-msg))))])
                  (syntax-parameterize
                      ([fail (make-rename-transformer #'fail-fn)])
                    body ...)
                  (test-result #f "")))]))]))


(module+ test
  (define-test (foobar? a b)
    (unless (equal? a (+ b 1))
      (fail "~a and ~a don't foobar!" a b)))

  (test-begin
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

  (test-begin
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

  ;; Test that test-begin ignore allows definitions
  (test-begin
    (equal? 1 1)
    (ignore (define a 42))
    (equal? a a)))
