#lang scribble/manual

@(require scribble/core)

@title{Ruinit}
@author[(author+email "Lukas Lazarek" "lukas.lazarek@eecs.northwestern.edu")]

@(define (yellow . content)
  (make-element (make-style #f (list (make-background-color-property "yellow")))
                content))

@nested[#:style 'inset]{
 @yellow{@bold{NOTE}}:
 This package is considered experimental and under active development.
 The things documented here may change, but the documentation will be
 kept up to date with any such changes.

}

@defmodule[ruinit]

@;;;;;;;;;;;;;;;@
@; Boilerplate ;@
@;;;;;;;;;;;;;;;@

@(require (for-label racket ruinit)
          scribble/example)

@(define ruinit-eval (make-base-eval))
@examples[#:eval ruinit-eval #:hidden (require racket ruinit)]

@;;;;;;;;;;;;;;;;;;;;;;;@
@; Begin documentation ;@
@;;;;;;;;;;;;;;;;;;;;;;;@

@section{Introduction}

Ruinit is a simple unit testing framework aimed at improving the
composability of tests while maintaining good failure reporting.

@section[#:tag "test-begin"]{Running tests}

Ruinit provides one primitive for checking test results called
@racket[test-begin]. This primitive plays the role of the test handler
which recognizes test outcomes and logs them as appropriate. Ruinit
does not employ special test functions which independently perform
checks and communicate results to the testing environment.

@defform[#:literals (ignore)
        (test-begin maybe-name
	             maybe-short-circuit
		     maybe-before/after ...
	  test-expr ...)
         #:grammar
         [(test-expr (code:line any-expr)
                     (ignore any-expr ...))
 	  (maybe-name (code:line)
	              (code:line #:name name))
          (maybe-short-circuit (code:line)
                               (code:line #:short-circuit))
          (maybe-before/after (code:line)
                              (code:line #:before before-expr)
                              (code:line #:after after-expr))]]{

  Opens a test block. All expressions outside of an @racket[ignore]
  form are considered tests: their results will be
  @seclink["test-outcomes"]{checked for failure}.

  Expressions inside @racket[ignore] forms will be spliced into the
  surrounding test block. This allows definitions and other setup code
  to appear inside the test block, without being considered tests.

  Providing a @racket[#:name] causes test failures inside the test
  block to be annotated with the given name.

  Specifying @racket[#:short-circuit] causes a test failure to prevent
  evaluation of all later tests in the block. The default is to
  evaluate every test in a block, even when a test fails.

  An arbitrary number of @racket[#:before] and @racket[#:after]
  clauses may be specified, each of which provides an expression to
  run before entering the dynamic extent of the test block or an
  expression to run upon exiting the dynamic extent of the block (in
  any way, including things like exceptions). Before and after clauses
  provide convenient mechanisms for setting up and cleaning up
  test environments.

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-begin
    (equal? (string-upcase "hello") "HELLO")
    (ignore (define four (+ 2 2)))
    (= four 4))

  (test-begin
    #:name foobar-test
    #:short-circuit
    #:before (displayln 'running-before!)
    #:after (displayln 'running-after!)
    (= 2 2)
    (< 10 1)
    (ignore (displayln 'never-happens))
    (= 5 "never happens due to short circuiting"))]

  Note that the source locations in the failure messages are not
  particularly meaningful in the context of examples, but in general
  they will always point to the source location of the top level test
  that failed (regardless of macro transformations etc).

}


@;;;;;;;;;;;;;;;;;@
@; Builtin tests ;@
@;;;;;;;;;;;;;;;;;@

@section[#:tag "tests"]{Tests}

Ruinit provides a number of simple tests with improved error reporting
for convenience. These tests produce
@racketlink[test-result?]{test-results} rather than booleans to
provide messages along with the test outcomes.

@defproc[(test-equal? [a any/c] [b any/c]) test-result?]{

  Tests that @racket[a] and @racket[b] are @racket[equal?]. This test
  also does some extra work to provide nice failure messages by
  pretty-printing and doing a very primitive diff of the values being
  compared. This extra behavior can be controlled with
  @racket[test-equal?-pretty-print-values] and
  @racket[test-equal?-diff-values].

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-begin
    (test-equal? (build-list 30 values)
                 (list-set (build-list 30 values) 15 1000)))]

}


@deftogether[(@defproc[(test-eq? [a any/c] [b any/c]) test-result?]
              @defproc[(test-eqv? [a any/c] [b any/c]) test-result?]
	      @defproc[(test-= [a number?] [b number?]) test-result?]
              @defproc[(test-< [a number?] [b number?]) test-result?]
              @defproc[(test-<= [a number?] [b number?]) test-result?]
              @defproc[(test-> [a number?] [b number?]) test-result?]
              @defproc[(test->= [a number?] [b number?]) test-result?])]{

  These tests all test their respectively named comparators from
  @racket[racket].

}

@defproc[(test-within [a number?] [b number?] [ε number?]) test-result?]{

  Tests that @racket[a] and @racket[b] are within @racket[ε] of each other.

}

@defform[(test-match pattern e)]{

  Tests that @racket[e] matches pattern @racket[pattern], as with
  @racket[match].

}

@defform[(test-exn predicate e)]{

  Tests that @racket[e] throws an exception recognized by @racket[predicate].

}



@;;;;;;;;;;;;;;;@
@; Combinators ;@
@;;;;;;;;;;;;;;;@


@section[#:tag "combinators"]{Test result combinators}

The following combinators provide convenient ways to build tests that
invoke other tests. They each behave in the same way as their
counterparts in @racket[racket], but extended to handle
@racketlink[test-result?]{test-results} as one might expect: a
@racketlink[test-fail?]{test failure} is treated analagously to
@racket[#f] and a success like @racket[#t].
These forms also respect the messages of their arguments when feasible.

@deftogether[(@defform[(and/test test-e ...)]
              @defform[(or/test test-e ...)]
              @defform[(not/test test-e)])]{

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-begin
    (and/test (test-= 1 1) (test-= 1 2) (test-= 1 3)))

  (test-begin
    (or/test (test-= 1 1) (test-= 1 2) (test-= 1 3)))

  (test-begin (not/test (test-= 1 1)))]

}

@deftogether[(@defform[(if/test condition e e)]
              @defform[(when/test condition e ...)]
              @defform[(unless/test condition e ...)])]{

}

@defform[(for/and/test for-clause body ...)]{

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-begin
    (for/and/test ([i (in-naturals)])
      (test-< i 5)))]

}

@defform[(and/test/message [test-e message] ...)]{

  Combines tests like @racket[and/test], but augments the message of a
  failing @racket[test-e] with the given @racket[message].

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-begin
    (and/test/message
      [(test-= (+ 1 1) 2) "I don't understand +:"]
      [(test-= (- 1 1) 2) "I don't understand -:"]))]


}


@deftogether[(@defform[(fail-when test-e)]
              @defform[(fail-unless test-e)])]{

  These forms provide a shorthand for checking a test result with
  @racket[when/test] and @racket[unless/test] followed by calling
  @racket[fail] and propogating the test message of @racket[test-e].

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (define-test (test-foo x)
    (fail-when (test-= x 2)))

  (test-begin
    (test-foo 3)
    (test-foo 2))]

}



@;;;;;;;;;;;;;;;@
@; Definitions ;@
@;;;;;;;;;;;;;;;@


@section[#:tag "definitions"]{Defining tests}

While any function can act as a test, ruinit provides a few forms that
make test definition more convenient.

@defform[(define-test (name formal ...)
            body ...+)]{

  Defines a test, providing bindings for two procedures within the
  test's @racket[body] expressions: @racket[fail] and
  @racket[succeed], which immediately abort a test with a failure or
  success respectively.

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (define-test (upcase-of? upper lower)
    (equal? upper (string-upcase lower)))

  (define-test (foobars? x y)
    (unless (= (* 2 (add1 x)) y)
      (fail "~v and ~v don't foobar!" x y)))

  (test-begin
    (upcase-of? "A" "a")
    (upcase-of? "A" "b")

    (foobars? 1 5)
    (foobars? 2 6))]
}

@defform[(define-simple-test (name formal ...)
                             maybe-success-message
                             maybe-fail-message
            body ...+)
         #:grammar
         [(maybe-success-message (code:line)
                                 (code:line #:success-message msg-string))
          (maybe-fail-message (code:line)
                              (code:line #:fail-message msg-string))]]{

  Defines a test that always produces a
  @racketlink[test-result?]{test-result} with an informative message
  on the success or failure of the test. The messages can be
  customized via the keyword arguments, but failing to provide them
  will cause a message to be automatically created.

  This form expands to @racket[define-test], so @racket[fail] and
  @racket[succeed] are available in the test's body.

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (define-simple-test (my= a b)
    (= a b))
  (define-simple-test (my=/with-messages a b)
    #:success-message (format "~v and ~v are =!" a b)
    #:fail-message "HAHAHA"
    (my= a b))

  (test-begin
    (my= 1 2)
    (my=/with-messages 1 2))
  (test-message (my= 1 1))
  (test-message (my=/with-messages 1 1))]
}

@defform[(define-test-syntax (name pat ...)
            body ...+)]{

  Defines a test macro. Any pattern that works for
  @racket[syntax-parse] may be used. The @racket[body]s should produce
  a syntax object, just like @racket[define-syntax].

  Just like @racket[define-test], the @racket[fail] and
  @racket[succeed] forms are available within the body of the test
  syntax.

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (define-test-syntax (fancy-test-syntax before-e ... {~datum :} after-e ...)
    #'(begin
        (fail-unless (and/test before-e ...))
	(unless/test (or/test after-e ...)
	  (fail "At least one after-e must succeed, but all failed"))))

  (test-begin
    (fancy-test-syntax (= 1 1) (equal? 'a 'a) : #f))]

}

@deftogether[(@defform[(fail [fmt-string fmt-arg ...])
		       #:contracts ([fmt-string string?]
				    [fmt-arg any/c])]
	      @defform[(succeed [fmt-string fmt-arg ...])
		       #:contracts ([fmt-string string?]
				    [fmt-arg any/c])])]{

  Aborts a test immediately with a failure or success, respectively.
  When given a @racket[fmt-string], it becomes the message associated
  with the outcome and acts as a @racket[format] string along with the
  @racket[fmt-arg]s.

  This form is a syntax error outside of the @racket[define-test],
  @racket[define-simple-test], and @racket[define-test-syntax] forms.

}



@;;;;;;;;;;;;;;;;;@
@; Test outcomes ;@
@;;;;;;;;;;;;;;;;;@


@section[#:tag "test-outcomes"]{Test outcomes}

The tests and combinators provided by ruinit generally return
@racketlink[test-result?]{test results}, which package together a
result value with extra information used in failure reporting. Ruinit
recognizes such values specially, but they are not the only things
that are recognized as test outcomes: any value can be a test result.

Two kinds of values are considered failure outcomes for a test:
@itemlist[
@item{@racket[#f], or}
@item{a value satisfying @racket[test-fail?] (such as produced by
      @racket[test-fail], or any of the
      @seclink["combinators"]{test combinators})}
]


@deftogether[(@defproc[(test-fail? [v any/c])
                       boolean?]
              @defproc[(test-success? [v any/c])
                       boolean?])]{

  Returns whether @racket[v] is @racket[#f] or a
  @racketlink[test-result?]{test-result} indicating test failure (or
  neither of the two for @racket[test-success?]).

  These functions are inverses; the result of one is the opposite of
  the other.

  @examples[#:eval ruinit-eval #:preserve-source-locations
  (test-fail? (test-= 1 1))
  (test-success? (test-= 1 1))
  (test-fail? (test-= 1 2))
  (test-success? (test-= 1 2))
  (test-fail? #f)
  (test-fail? #t)
  (test-fail? '(1 2 3))]

}

@deftogether[(@defproc[(test-fail [msg-fmt-string (or/c #f string?)]
                                  [fmt-arg any/c]
                                  ...)
                       (and/c test-result? test-fail?)]
              @defproc[(test-success [msg-fmt-string (or/c #f string?)]
                                     [fmt-arg any/c]
                                     ...)
                       (and/c test-result? test-success?)])]{

  Produces a @racketlink[test-result?]{test-result} indicating
  test-failure (or success, respectively) with the given message,
  which is treated as a format string (see @racket[format]) string
  along with the given @racket[fmt-arg]s.

}


@defproc[(test-result? [v any/c])
         boolean?]{

  Recognizes test results, such as produced by @racket[test-fail],
  @racket[test-success], and others.

}

@defproc[(test-message [v any/c])
         (or/c #f string?)]{

  Extracts the message from @racket[v] if it is a
  @racketlink[test-result?]{test-result} with a message. Otherwise
  produces @racket[#f].

}

@defproc[(extend-test-message [result test-result?] [fmt-str string?] [fmt-arg any/c] ...
                              [#:append? append? boolean? #t])
         test-result?]{

  Extends the message of @racket[result] with the string produced by
  @racket[(format fmt-str fmt-arg ...)]. The optional keyword
  @racket[#:append?] controls whether the extension happens at the end
  or the beginning of the message.

}




@;;;;;;;;;;@
@; Config ;@
@;;;;;;;;;;@


@section[#:tag "config"]{Configurable options}

@defparam[test-equal?-pretty-print-values bool boolean?
          #:value #t]{

  Controls whether @racket[test-equal?] pretty-prints values in failure messages.

}

@defparam[test-equal?-diff-values bool boolean?
          #:value #t]{

  Controls whether @racket[test-equal?] diffs values in failure messages.

}

@defparam[max-code-display-length length natural?
          #:value 70]{

  Controls the maximum length (in characters) of test syntax to
  display in failure messages.

}

@defparam[use-rackunit-backend bool boolean?
          #:value #f]{

  Controls whether test results are handled via rackunit or by
  ruinit's own handlers, which display messages in a slightly
  different format more tailored to ruinit's structure.

  Regardless of the value of this parameter, test results will be
  logged for working nicely with @racket[raco test].

}
