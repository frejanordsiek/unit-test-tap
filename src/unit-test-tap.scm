;;; unit-test-tap.scm --- TAP output unit-tests in the spirit of SRFI-64
;;; coding: utf-8

;;; unit-test-tap - scheme unit testing framework with TAP output
;;; Copyright (C) 2016 Freja Nordsiek
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA  02110-1301  USA



;;; A TAP output based unit testing framework that is in the spirit of
;;; SRFI-64 (http://srfi.schemers.org/srfi-64/srfi-64.html). The
;;; interface is similar and many procedures have the same names.
;;; However, skip and expected fail functionality are included into
;;; the tests themselves as keyword arguments instead of as separate
;;; procedures/macros. Additionally, the test-name argument has been
;;; moved to the end instead of being the first argument of each
;;; test macro, so as to better work with custom optional and
;;; keyword argument facilities. Another major difference is that
;;; test-begin's cannot be nested, though test-group-begin does
;;; add one level of nesting where all the tests within a group count
;;; as a single test for TAP output purposes (first one to FAIL or
;;; XPASS fails the whole group).
;;;
;;; This module is not portable outside of Guile without some modification.
;;; It uses Common Lisp style docstrings and a Guile specific module/library
;;; declaration. The source package can make R6RS and R7RS versions of this
;;; module/library by stripping these out and putting in R6RS and R7RS
;;; library declarations.


(define-module (unit-test-tap)
  #:pure
  #:version (0 1)
  #:duplicates (check)
  #:use-module ((rnrs base) #:version (6))
  #:use-module ((rnrs control) #:version (6))
  #:use-module ((rnrs lists) #:version (6))
  #:use-module ((rnrs io ports) #:version (6))
  #:use-module ((rnrs io simple) #:version (6))
  #:use-module ((rnrs exceptions) #:version (6))
  #:use-module ((rnrs programs) #:version (6))
  #:export (test-port test-yaml-prefix test-count test-number
                      test-number-passed test-number-failed
                      test-number-xfailed test-number-xpassed
                      test-number-skipped
                      test-group-name test-group-failed
                      increment-test-counter
                      test-begin test-end
                      test-group-begin test-group-end
                      test-group-with-cleanup
                      test-pred
                      test-assert test-eq test-eqv test-equal
                      test-approximate test-error))


;;; Define SRFI-1 iota
(define iota
  (lambda (n)
    (let loop ((m (- n 1)) (lst '()))
      (if (= m -1)
          lst
          (loop (- m 1) (cons m lst))))))

;; Short utility procedure to check if a string is 0 length.
(define string-null?
  (lambda (s) (= 0 (string-length s))))

;;; Returns the given alist but with the entry for obj removed. The order of
;;; the other entries is reversed.
(define alist-remove
  (lambda (obj alist)
    (let loop ((new-alist '()) (al alist))
      (if (null? al)
          new-alist
          (loop (if (eq? obj (car (car al)))
                    new-alist
                    (cons (car al) new-alist))
                (cdr al))))))

;;; Process optional and keyword arguments, both with default values,
;;; where the keys are symbols. An alist of of argument-value pairs is
;;; returned, where arguments not given are set to their defaults.
;;; optionals should be alist of the optional argument names and their
;;; default values. keyword-args should be the same. Then the actual
;;; arguments to process are given as arguments.
(define process-optional-args
  (lambda (optionals keywords arguments)
    "- Scheme Procedure: process-optional-args optionals keywords
                                          arguments
     Procedure to process the optional and keyword arguments contained
     in ARGUMENTS where symbols are used as keywords. The optional
     and keyword arguments to find must be given in OPTIONALS and
     KEYWORDS respectively, which must be alists of argument name,
     default value pairs. The results are returned as an alist
     of argument name-value pairs with values set to defaults for
     arguments that were not given."
    (let opt-loop ((alist '()) (opt-args optionals) (args arguments))
      ;; No more arguments to process.
      (cond ((null? args) (append alist opt-args keywords))
            ;; Done processing optional arguments - move on to keyword arguments.
            ((or (null? opt-args) (assoc (car args) keywords))
             ;; Must have an even number of remaining arguments and not have more than
             ;; available options.
             (if (or (odd? (length args)) (> (length args) (* 2 (length keywords))))
                 (raise 'invalid-args)
                 (let key-loop ((al (append alist opt-args)) (key-args keywords) (rargs args))
                   ;; No more arguments to process
                   (if (null? rargs)
                       (append al key-args)
                       ;; Check that the symbol for the current key-value pair is valid and
                       ;; if it is, take it and go onto the next pair.
                       (let ((key (car rargs))
                             (val (car (cdr rargs)))
                             (others (cdr (cdr rargs))))
                         (if (not (assoc key key-args))
                             (raise 'invalid-args)
                             (key-loop (cons (cons key val) al)
                                       (alist-remove key key-args)
                                       others)))))))
            ;; Another optional argument to process.
            (else (opt-loop (cons (cons (car (car opt-args)) (car args)) alist)
                            (cdr opt-args)
                            (cdr args)))))))


;;; Set the variables to hold the port, prefix for yaml blocks,
;;; number of tests, number of passed/failed/xfailed/xpassed tests, the
;;; test group name, and whether a test in the group has failed as well
;;; as give functions to retrieve them.
(define *test-port* (current-output-port))
(define *test-yaml-prefix* "")
(define *test-number* 0)
(define *test-count* 1)
(define *test-number-passed* 0)
(define *test-number-failed* 0)
(define *test-number-xfailed* 0)
(define *test-number-xpassed* 0)
(define *test-number-skipped* 0)
(define *test-group-name* "")
(define *test-group-failed* #f)

;;; Increments the specified counter, or int he case of
;;; test-group-failed, sets it to #t
(define increment-test-counter
  (lambda (key)
    "- Scheme Procedure: increment-test-counter key
     Increments the counter whose name is in the string KEY. For
     \"test-group-failed\", it is set to #t. This procedure is mostly
     meant for internal use, but has to be exported for the test macros
     to use on some scheme implementations."
    (cond
      ((string=? key "test-count") (set! *test-count* (+ 1 *test-count*)))
      ((string=? key "test-number") (set! *test-number* (+ 1 *test-number*)))
      ((string=? key "test-number-passed") (set! *test-number-passed* (+ 1 *test-number-passed*)))
      ((string=? key "test-number-failed") (set! *test-number-failed* (+ 1 *test-number-failed*)))
      ((string=? key "test-number-xfailed") (set! *test-number-xfailed* (+ 1 *test-number-xfailed*)))
      ((string=? key "test-number-xpassed") (set! *test-number-xpassed* (+ 1 *test-number-xpassed*)))
      ((string=? key "test-number-skipped") (set! *test-number-skipped* (+ 1 *test-number-skipped*)))
      ((string=? key "test-group-failed") (set! *test-group-failed* #t))
      (else (raise 'invalid-argument)))))


(define test-port
  (lambda ()
    "- Scheme Procedure: test-port
     Return the port that the test output is written to."
    *test-port*))
(define test-yaml-prefix
  (lambda ()
    "- Scheme Procedure: test-yaml-prefix
     Return the string prefix for the yaml diagnostic block lines written
     after FAIL, XFAIL, and XPASS tests."
    *test-yaml-prefix*))
(define test-count
  (lambda ()
    "- Scheme Procedure: test-count
     Return the 1 based index (integer) of the current test to be done."
    *test-count*))
(define test-number
  (lambda ()
    "- Scheme Procedure: test-number
     Return the total number (integer) of tests that are set to be
     done, as set by the procedure test-begin."
    *test-number*))
(define test-number-passed
  (lambda ()
    "- Scheme Procedure: test-number-passed
     Return the total number (integer) of tests that have passed so far."
    *test-number-passed*))
(define test-number-failed
  (lambda ()
    "- Scheme Procedure: test-number-failed
     Return the total number (integer) of tests that have failed so far."
    *test-number-failed*))
(define test-number-xfailed
  (lambda ()
    "- Scheme Procedure: test-number-xfailed
     Return the total number (integer) of tests that have xfailed so far."
    *test-number-xfailed*))
(define test-number-xpassed
  (lambda ()
    "- Scheme Procedure: test-number-xpassed
     Return the total number (integer) of tests that have xpassed so far."
    *test-number-xpassed*))
(define test-number-skipped
  (lambda ()
    "- Scheme Procedure: test-number-skipped
     Return the total number (integer) of tests that have been skipped
     so far."
    *test-number-skipped*))
(define test-group-name
  (lambda ()
    "- Scheme Procedure: test-group-name
     Return the name (string) of the currently entered group (\"\"
     if none)."
    *test-group-name*))
(define test-group-failed
  (lambda ()
    "- Scheme Procedure: test-group-failed
     Return whether the currently entered group has failed (one FAIL or
     XPASS test inside it) as a boolean."
    *test-group-failed*))



;;; Indicate the test result which is
;;;
;;; "ok COUNT - NAME"
;;;
;;; or
;;;
;;; "not ok COUNT - NAME"
;;;
;;; depending on whether the test passed (ok) or not. " # SKIP" or
;;; " # TODO" are then appended on the same line if the test was
;;; supposed to be skipped or was expected to fail.
;;;
;;; This is then followed by each string in MSG on its own line,
;;; prefixed by *test-yaml-prefix*.
(define indicate-result
  (lambda (count name ok skip xfail msg)
    "- Scheme Procedure: indicate-result count name ok skip xfail msg
     Write the specified test results to output."
    (display (if ok "ok " "not ok ") *test-port*)
    (display count *test-port*)
    (if (not (string-null? name))
        (display (string-append " - " name) *test-port*))
    (if (or skip xfail)
        (display (string-append " # " (if skip "SKIP" "TODO")) *test-port*))
    (newline *test-port*)
    (let ((proc (lambda (m)
                  (display (string-append *test-yaml-prefix* m) *test-port*)
                  (newline *test-port*))))
      (for-each proc msg))
    (flush-output-port *test-port*)))


;;; Create the failure messaging block, which is in the form of
;;;
;;; "  ---"
;;;
;;; followed by each entry in lines. Each entry needs to be a three
;;; element improper list having the number of times to indent relative
;;; to the initial indentation, the key, and the value. Value is run
;;; through format. Each line will look like
;;; "    KEY: VALUE"
;;;
;;; Followed by
;;;
;;; "  ..."
(define failure-message-block
  (lambda (lines)
    "- Scheme Procedure: failure-message-block lines
     Return the list of string lines for a failure block given the
     the specification for what should be on each line in LINES."
    (if (null? lines) '()
        (append '("  ---")
                (map (lambda (c)
                       (let ((entry (cdr c)))
                         (string-append
                          (make-string (* 2 (+ 1 (car c))) #\space)
                          (car entry) ": "
                          (call-with-string-output-port
                           (lambda (port) (display (cdr entry) port))))))
                     lines)
                '("  ...")))))


;;; Format a list of args, exprs, etc. for use in a failure block with
;;; a specified indentation level and prefix for each element name
;;; (e.g. "arg" for "arg0" ... "argN").
(define format-listing
  (lambda (level prefix args)
    "- Scheme Procedure: format-listing level prefix args
     Return the line descriptions for each argument."
    (map (lambda (i arg)
           (cons* level
                  (string-append prefix (number->string i))
                  arg))
         (iota (length args)) args)))


;;; Wrap a test function, handling the incrementing of count as well as
;;; number-failed if the test failed, handling the optional name for the
;;; test, and whether it should be skipped or not. Error catching is done
;;; if proc does not do any catching itself. Name is handled as an
;;; optional positional argument while skip and xfail are handled as
;;; keyword arguments.
(define-syntax wrap-test
  (syntax-rules ()
    "- Scheme Macro: (wrap-test (proc . args) args-skip-fail . extra-args)
     Wrap the test macro/procedure PROC passing it ARGS to handle
     parsing EXTRA-ARGS for optional and keyword arguments, incrementing
     various counters, doing last resort exception handling if the
     test doesn't handle it, handling XPASS results, and writing the
     test results to output."
    ((wrap-test (proc . args) args-skip-fail . extra-args)
     ;; Handle the optional name argument and keyword skip and xfail
     ;; arguments.
     (let* ((arg-alist (process-optional-args '((name . "")) '((skip . #f) (xfail . #f))
                                              (list . extra-args)))
            (xname (cdr (assoc 'name arg-alist)))
            (xskip (cdr (assoc 'skip arg-alist)))
            (xxfail (cdr (assoc 'xfail arg-alist))))
       ;; Completely skip if in a group that has already had a failure
       (if (or (string-null? (test-group-name)) (not (test-group-failed)))
           ;; The result will be put in msg, which will be null if PASS.
           (let ((msg '())
                 (fullname (if (string-null? (test-group-name)) xname
                               (string-append (test-group-name) "->" xname))))
             ;; Operate the test and put the output in msg if it isn't to
             ;; be skipped. If an error is caught, the test is a FAIL
             ;; and the error message must be generated.
             (guard
                 (key
                  (#t
                   (set! msg (failure-message-block
                              (append
                               (list (cons* 0 "message" "Error thrown")
                                     (cons* 0 "error"
                                            (call-with-string-output-port
                                             (lambda (p) (display key p))))
                                     (cons* 0 "got" ""))
                               (format-listing
                                1 "expr"
                                (list-tail 'args args-skip-fail)))))))
               (if (not xskip)
                   (set! msg (proc . args))))
             (let ((passed (null? msg)))
               ;; If we were expected to fail but passed, then msg
               ;; needs to be set to a failure block of the arguments.
               (if (and xxfail passed)
                   (set! msg
                     (failure-message-block
                      (append
                       (list (cons* 0 "message"
                                    "Expected to fail but didn't")
                             (cons* 0 "got" ""))
                       (format-listing 1 "expr"
                                       (list-tail 'args args-skip-fail))))))
               ;; For null messages (passed and were supposed to) or
               ;; for expected failures that successfully resulted in
               ;; failure, the result is indicated (if not in a group)
               ;; and the passed or xfailed counters incremented
               ;; appropriately.
               ;;
               ;; Otherwise, the test was either a failure or an
               ;; unexpected pass (expected failure but passed). The
               ;; retulst is indicated and the failure or xpassed
               ;; counters incremented appropriately. If inside a group,
               ;; the group is set to failed.
               (if (or (null? msg) (and xxfail (not passed)))
                   (if (string-null? (test-group-name))
                       (begin
                         (indicate-result (test-count) xname
                                          passed xskip xxfail msg)
                         (cond (xxfail
                                (increment-test-counter "test-number-xfailed"))
                               (xskip
                                (increment-test-counter "test-number-skipped"))
                               (else
                                (increment-test-counter "test-number-passed")))))
                   (begin
                     (indicate-result (test-count) fullname
                                      passed xskip xxfail msg)
                     (if xxfail
                         (increment-test-counter "test-number-xpassed")
                         (increment-test-counter "test-number-failed"))
                     (if (not (string-null? (test-group-name)))
                         (increment-test-counter "test-group-failed")))))
             ;; If not in a group, this test counts against the total
             ;; number.
             (if (string-null? (test-group-name))
                 (increment-test-counter "test-count"))))))))


;;; Raw test macro for applying a predicate to one or more arguments
;;; and seeing whether the output is not false or false. If it is
;;; false, a message is generated giving the unevaluated and evaluated
;;; arguments. The string name for the predicate must also be given.
;;; It can work with predicates that take any number of arguments,
;;; including zero.
(define-syntax lowlevel-test-pred
  (syntax-rules ()
    "- Scheme Macro: (lowlevel-test-pred pred pred-name . exprs)
     Asserts that (PRED . EXPRS) evaluates non #f and returns the
     message lines of the result (null means PASS). Exceptions in
     evaluating each expression in EXPRS and in evaluating PRED
     are caught. Predicates that take any number of arguments,
     including zero, are supported."
    ((lowlevel-test-pred pred pred-name . exprs)
     ;; The result messages will be returned by the continuation, which
     ;; will be null if PASS. The values of each expression passed need
     ;; to be obtained. If an error is thrown, error msgs indicating the
     ;; error and what the unevaluated inputs were is generated.
     (call-with-current-continuation
      (lambda (return)
        (let ((values (guard
                          (key
                           (#t (return
                                (failure-message-block
                                 (append
                                  (list
                                   (cons*
                                    0 "message"
                                    "Error thrown evaluating expressions")
                                   (cons* 0 "error"
                                          (call-with-string-output-port
                                           (lambda (p) (display key p))))
                                   (cons* 0 "got" ""))
                                  (format-listing 1 "expr" 'exprs))))))
                        (list . exprs))))
          ;; Pred is applied to the values and null returned if the
          ;; result is logical true (anything but #f) and the FAIL
          ;; message otherwise (gives unevaluated and evaluated
          ;; arguments). The evaluation is done with exception catching.
          ;; An exception is considered FAIL.
          (guard
              (key (#t (failure-message-block
                        (append
                         (list (cons*
                                0 "message"
                                (string-append "Error thrown applying "
                                               pred-name))
                               (cons* 0 "error"
                                      (call-with-string-output-port
                                       (lambda (p) (display key p))))
                               (cons* 0 "got" ""))
                         (format-listing 1 "expr" 'exprs)
                         (list (cons* 0 "evaluated" ""))
                         (format-listing 1 "arg" values)))))
            (if (apply pred values) '()
                (failure-message-block
                 (append
                  (list (cons* 0 "message"
                               (string-append
                                "Arguments did not evaluate "
                                pred-name))
                        (cons* 0 "got" ""))
                  (format-listing 1 "expr" 'exprs)
                  (list (cons* 0 "evaluated" ""))
                  (format-listing 1 "arg" values)))))))))))


;;; Raw test macro checking whether an expression throws a particular
;;; kind of error (or any error if set to #t).
(define-syntax lowlevel-test-error
  (syntax-rules ()
    "- Scheme Macro: (lowlevel-test-error error-type expr)
     Asserts EXPR throws an exception of type ERROR-TYPE when
     evaluated. If an exception is not caught or a different one
     is caught, the test is considered a failure. The resulting
     message lines are returned (null means PASS)."
    ((lowlevel-test-error error-type expr)
     ;; The result messages will be return, which will be null if PASS.
     ;; error-type and expr are converted to string to help make
     ;; failure blocks.
     (let ((s-expr (call-with-string-output-port
                    (lambda (p) (display 'expr p))))
           (s-error-type (call-with-string-output-port
                          (lambda (p) (display error-type p)))))
       ;; Catch all errors and check to see if one is thrown and that it
       ;; matches error-type. If it does, the test is a PASS. Otherwise
       ;; it is a FAIL.
       (guard
           (key
            ((or (eq? error-type #t) (equal? error-type key)) '())
            (else (failure-message-block
                   (list (cons* 0 "message" "Wrong exception was thrown")
                         (cons* 0 "error" "")
                         (cons* 1 "got" (call-with-string-output-port
                                         (lambda (p)
                                           (display key p))))
                         (cons* 1 "expected" s-error-type)
                         (cons* 0 "got" "")
                         (cons* 1 "expr0" s-expr)))))
         expr
         (failure-message-block
          (list (cons* 0 "message" "Exception was not thrown")
                (cons* 0 "error" s-error-type)
                (cons* 0 "got" "")
                (cons* 1 "expr0" s-expr))))))))


;;; Begin testing for n tests, by setting all the variables to initial
;;; values and outputting the TAP header.
(define test-begin
  (lambda (n . args)
    "- Scheme Procedure: test-begin n ['port p] ['yaml-prefix prefix]
     Starts the unit testing framework/suite that is expected to have
     N tests. All counters are reset and the TAP header is written to
     output. The test writes its output to (current-output-port), or
     P if it is given (it is expected to be an already opened output
     port). The YAML lines in the diagnostic messages for FAIL, XFAIL,
     and XPASS tests are prefixed with PREFIX if it is given. End
     testing with (test-end)."
    ;; Turn args into an alist of the key values and thrown an error if
    ;; we don't have the right number of pairs (0, 1, 2) or if there
    ;; is an invalid key. Then, extract port and yaml-prefix and set their
    ;; globals.
    (let ((arg-alist (process-optional-args
                      '()
                      (list (cons 'port (current-output-port)) (cons 'yaml-prefix ""))
                      args)))
      (set! *test-port* (cdr (assoc 'port arg-alist)))
      (set! *test-yaml-prefix* (cdr (assoc 'yaml-prefix arg-alist))))
    (set! *test-number* n)
    (set! *test-count* 1)
    (set! *test-number-passed* 0)
    (set! *test-number-failed* 0)
    (set! *test-number-xfailed* 0)
    (set! *test-number-xpassed* 0)
    (set! *test-number-skipped* 0)
    (set! *test-group-name* "")
    (set! *test-group-failed* #f)
    (display "TAP version 13" *test-port*)
    (newline *test-port*)
    (display (string-append "1.." (number->string n)) *test-port*)
    (newline *test-port*)
    (flush-output-port *test-port*)))


;;; End testing, which causes an exit with the status determined
;;; by whether any test so far has failed or not.
(define test-end
  (lambda ()
    "- Scheme Procedure: test-end
     Exits the interpreter/environment with an exit code set to
     whether the expected number of tests were run and their results
     were all PASS, XFAIL, and/or SKIP (exit code of 0) or not (exit
     code of 1)."
    (exit (and (= 0 (+ *test-number-failed* *test-number-xpassed*))
               (= (+ 1 *test-number*) *test-count*)))))


;;; Begin a group of tests
(define test-group-begin
  (lambda (name)
    "- Scheme Procedure: test-group-begin name
     Begins a test group with the given NAME. An error is thrown
     if NAME is not a string or group has already been begun. All
     tests within a group pass or fail together as if they were a
     single test. End the group with (test-group-end)."
    (cond ((not (string? name))
           (raise 'test-group-begin))
          ((not (string-null? *test-group-name*))
           (raise 'test-group-begin))
          (else (begin
                  (set! *test-group-name* name)
                  (set! *test-group-failed* #f))))))


;;; End a group of tests
(define test-group-end
  (lambda ()
    "- Scheme Procedure: test-group-end
     Ends the current test group. If all tests in it passed, the
     group is considered to be a PASS."
    (if (string-null? *test-group-name*)
        (raise 'test-group-end)
        (begin
          (if (not *test-group-failed*)
              (begin
                (indicate-result *test-count* *test-group-name* #t #f #f '())
                (set! *test-number-passed* (+ 1 *test-number-passed*))))
          (set! *test-count* (+ *test-count* 1))
          (set! *test-group-name* "")))))


;;; Wrap forms/expressions around in a group with a
;;; cleanup form/expression that is evaluated at the end, even if there
;;; is an error.
(define-syntax test-group-with-cleanup
  (syntax-rules ()
    "- Scheme Macro: (test-group-with-cleanup name expr cleanup-form)
     Enters the group NAME, runs expression EXPR, and then ends the
     group and executes CLEANUP-FORM regardless of whether an
     exception occured or not in beginning the group or running EXPR."
    ((test-group-with-cleanup name expr cleanup-form)
     (begin
       (dynamic-wind
         (lambda ()
           (if (string-null? (test-group-name))
               (guard (key (#t #t)) (test-group-begin name))))
         (lambda () (guard (key (#t #t)) expr))
         (lambda () (guard (key (#t #t)) cleanup-form)))
       (if (not (string-null? (test-group-name)))
           (test-group-end))))))


;;; The different tests. The extra name, skip, and xfail arguments are
;;; processed by wrap-test.

;;; Main generic test, which takes a procedure/predicate and applies it
;;; to any number of arguments and sees if the result is logically
;;; true.
(define-syntax test-pred
  (syntax-rules ()
    "- Scheme Macro: (test-pred (pred . args) [name] ['skip skip]
                           ['xfail xfail])
     Asserts that the evaluation of (PRED . ARGS) is non #f. The
     test takes an optional NAME. Indicate whether the test should
     be skipped or is expected to fail with the keyword arguments
     'skip and 'xfail set to non #f (the default is #f). Predicates that
     take any number of arguments, including zero, are supported. An
     example test to see if the result of two mathematical expressions
     are equal but that one expects it to fail would be

          (test-pred (= (* 2 3) (* 2 4)) \"example\" 'xfail #t)"
    ((test-pred (pred . args) . extra-args)
     (wrap-test (lowlevel-test-pred pred (symbol->string 'pred) . args)
                2 . extra-args))))


;;; Test to catch specified errors
(define-syntax test-error
  (syntax-rules ()
    "- Scheme Macro: (test-error error-type expr [name] ['skip skip]
                            ['xfail xfail])
     Asserts that evaluating EXPR throws an exception of type
     ERROR-TYPE (set to #t to indicate any exception type).
     Throwing no exception or an exception of a different type is
     considered a failure. The test takes an optional NAME. Indicate
     whether the test should be skipped or is expected to fail with
     the keyword arguments 'skip and 'xfail set to non #f (the
     default is #f)."
    ((test-error error-type expr . extra-args)
     (wrap-test (lowlevel-test-error error-type expr)
                1 . extra-args))))


;;; Convenience test that checks if the given expression evaluates to
;;; logical true.
(define-syntax test-assert
  (syntax-rules ()
    "- Scheme Macro: (test-assert expr [name] ['skip skip]
                             ['xfail xfail])
     Asserts that EXPR evaluates to non #f. The test takes an optional
     NAME. Indicate whether the test should be skipped or is expected
     to fail with the keyword arguments 'skip and 'xfail set to
     non #f (the default is #f)."
    ((test-assert expr . extra-args)
     (wrap-test (lowlevel-test-pred (lambda (x) x) "#t" expr)
                2 . extra-args))))


;;; Convenience test that does eq? with two arguments through test-pred
(define-syntax test-eq
  (syntax-rules ()
    "- Scheme Macro: (test-eq expr0 expr1 [name] ['skip skip]
                         ['xfail xfail])
     Asserts (eq? EXPR0 EXPR1). The test takes an optional NAME.
     Indicate whether the test should be skipped or is expected
     to fail with the keyword arguments 'skip and 'xfail set to
     non #f (the default #f)."
    ((test-eq expr0 expr1 . extra-args)
     (test-pred (eq? expr0 expr1) . extra-args))))


;;; Convenience test that does eqv? with two arguments through test-pred
(define-syntax test-eqv
  (syntax-rules ()
    "- Scheme Macro: (test-eqv expr0 expr1 [name] ['skip skip]
                          ['xfail xfail])
     Asserts (eqv? EXPR0 EXPR1). The test takes an optional NAME.
     Indicate whether the test should be skipped or is expected
     to fail with the keyword arguments 'skip and 'xfail set to
     non #f (the default is #f)."
    ((test-eqv expr0 expr1 . extra-args)
     (test-pred (eqv? expr0 expr1) . extra-args))))


;;; Convenience test that does equal? with two arguments through test-pred
(define-syntax test-equal
  (syntax-rules ()
    "- Scheme Macro: (test-equal expr0 expr1 [name] ['skip skip]
                            ['xfail xfail])
     Asserts (equal? EXPR0 EXPR1). The test takes an optional NAME.
     Indicate whether the test should be skipped or is expected
     to fail with the keyword arguments 'skip and 'xfail set to
     non #f (the default #f)."
    ((test-equal expr0 expr1 . extra-args)
     (test-pred (equal? expr0 expr1) . extra-args))))


;;; Convenience test to check if two expressions evaluate to numbers
;;; that are within tolerance of each other.
(define-syntax test-approximate
  (syntax-rules ()
    "- Scheme Macro: (test-approximate expr0 expr1 tolerance [name]
                                  ['skip skip] ['xfail xfail])
     Asserts that the absolute value of the numerical difference between
     EXPR0 and EXPR1 is less than or equal to TOLERANCE. The test takes
     an optional NAME. Indicate whether the test should be skipped or is
     expected to fail with the keyword arguments 'skip and 'xfail set
     to non #f (the default #f)."
    ((test-approximate expr0 expr1 tolerance . extra-args)
     (wrap-test (lowlevel-test-pred
                 (lambda (x y) (<= (abs (- x y)) tolerance))
                 (string-append "approximately equal (within "
                                (number->string tolerance)
                                " of each other)")
                 expr0 expr1)
                2 . extra-args))))
