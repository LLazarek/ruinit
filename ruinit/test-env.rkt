#lang racket

(require syntax/parse/define
         racket/file
         racket/runtime-path
         (for-syntax racket/base))

(provide define-test-env)

(define-simple-macro (define-test-env (setup:id cleanup:id)
                       #:directories ([dir:id dirpath:expr] ...)
                       #:files ([filename:id path:expr contents] ...)
                       {~optional {~and #:provide provide-kw}})
  #:with maybe-provides (if (attribute provide-kw)
                            #'(provide filename ... dir ...)
                            #'(void))
  #:do [(define (replace-srcloc def)
          (define parts (syntax-e def))
          (datum->syntax this-syntax parts this-syntax))]
  #:with [dir-def ...] (map replace-srcloc
                            (syntax-e
                             #'[(define-runtime-path dir dirpath) ...]))
  (begin
    dir-def ...
    (define filename (simple-form-path path)) ...
    (define (setup)
      (cleanup)
      (make-directory dir) ...
      (display-to-file contents filename) ...)
    (define (cleanup)
      (for ([f (in-list (list filename ...))])
        (when (file-exists? f)
          (delete-file f)))
      (for ([d (in-list (list dir ...))])
        (when (directory-exists? d)
          (delete-directory/files d))))
    maybe-provides))
