;;; tests-base.scm --- test the unit-test-tap module
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


;;; Tests the unit-test-tap module. TAP output for this test is
;;; generated manually while unit-test-tap's output is put into
;;; string ports to be evaluated for correctness.

(import (unit-test-tap))


;;; Define SRFI-1 iota for implementations that won't have it in the default
;;; environment
(define iota
  (lambda (n)
    (let loop ((m (- n 1)) (lst '()))
      (if (= m -1)
          lst
          (loop (- m 1) (cons m lst))))))

;;; Helper function to basically get the same result as ice-9 format
;;; but using string ports instead.
(define-syntax my-format
  (syntax-rules ()
    ((my-format fmt obj)
     (call-with-string-output-port
       (lambda (p) (if (string= fmt "~a")
                       (display obj p)
                       (write obj p)))))))

;;; Get the newline character
(define newline-char (call-with-string-output-port newline))

;;; Get a header maker
(define TAP-header
  (lambda (n)
    (string-append "TAP version 13"
                   newline-char
                   "1.."
                   (number->string n)
                   newline-char)))

;;; Get a response maker
(define response
  (lambda (count name ok)
    (string-append (if ok "ok " "not ok ")
                   (number->string count)
                   " - "
                   name
                   newline-char)))

;;; Make a function that returns the current test count and increments
;;; it every time.
(define get-count
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;;; Make a function that gets the number of passed, failed, xfailed,
;;; xpassed, and skipped and puts them in one list.
(define get-test-counts
  (lambda ()
    (list *test-number-passed* *test-number-failed*
          *test-number-xfailed* *test-number-xpassed*
          *test-number-skipped*)))

;;; Make a function that checks the number of passed, failed, xfailed,
;;; xpassed, and skipped.
(define check-test-counts
  (lambda (passed failed xfailed xpassed skipped)
    (let ((t-counts (list passed failed xfailed xpassed skipped)))
      (and
       (for-all = (get-test-counts) t-counts)
       (= (+ 1 (apply + t-counts)) *test-count*)))))


;;; Output the header
(display (TAP-header 50))


;;; Test the TAP headers
(display (response (get-count) "header"
                   (for-all (lambda (n)
                              (string= (TAP-header n)
                                       (call-with-string-output-port
                                         (lambda (p)
                                           (test-begin n 'port p)))))
                            (map (lambda (x) (+ 1 x))
                                 (iota 10)))))

;;; Test initialization of variables by test-begin for a random
;;; number of tests and a prefix that is the conversion of that random
;;; number to string. The header is also checked.
(display (response (get-count) "initialization of variables"
                   (let ((number 10))
                     (for-all
                      (lambda (n prefix)
                        (let ((pass #f))
                          (let-values (((p get-output)
                                        (open-string-output-port)))
                            (test-begin n 'port p 'yaml-prefix prefix)
                            (set! pass
                              (and (string=? (TAP-header n)
                                             (get-output))
                                   (eq? p *test-port*)
                                   (string=? prefix *test-yaml-prefix*)
                                   (= n *test-number*)
                                   (check-test-counts 0 0 0 0 0)
                                   (string-null? *test-group-name*)
                                   (not *test-group-failed*)))
                            (close-port p))
                          pass))
                      (map (lambda (x) (+ 1 x)) (iota number))
                      (map (lambda (x) (number->string x)) (iota number))))))

;;; Macro to check test-assert/eq/eqv/equal/approximate for a single
;;; state given two lists of of expressions to test (second one ignored
;;; for assert).
(define-syntax check-asserts-state
  (syntax-rules ()
    ((check-asserts-state which state name prefix tol exprs0 exprs1)
     ;; The total output will be kept track of, msg used to hold failure
     ;; messages if any, the counts of pass/fail/etc., and the index
     ;; into counts for the state being tested (needed for incrementing).
     (let* ((number (length (list . exprs0)))
            (output (TAP-header number))
            (counts '(0 0 0 0 0))
            (msg '())
            (total (apply + counts))
            (index (cdr (assoc state '(("PASS" . 0) ("FAIL" . 1)
                                       ("XFAIL" . 2) ("XPASS" . 3)
                                       ("SKIP" . 4)
                                       ("EVAL EXCEPTION" . 1))))))
       (let-values (((p get-output) (open-string-output-port)))
         (test-begin number 'port p 'yaml-prefix prefix)
         ;; The test is the iteration over all the expressions till there
         ;; is a failure
         (display
          (response
           (get-count) (string-append "test-" which " " state)
           ;; Loop over the expressions incrementing the count each time.
           ;; Reaching null means all tests passed.
           (let loop ((count 1) (items0 (list . exprs0))
                      (items1 (list . exprs1)))
             (if (null? items1) #t
                 ;; Grab the first items and do the assertion. Then generate
                 ;; what the output should be, check the output, and check
                 ;; the counts for each kind of test.
                 (let* ((x (if (string= state "EVAL EXCEPTION")
                               '(excpt-raise 'aive)
                               (car items0)))
                        (y (if (string= state "EVAL EXCEPTION")
                               '(excpt-raise 'aive)
                               (car items1)))
                        (new-total (+ total count))
                        ;; Calculate the expected counts
                        (new-counts (map (lambda (y n) (if (= n index)
                                                           (+ y count)
                                                           y))
                                         counts (iota 5)))
                        ;; Generate the got/evaluated diagnostic lines for
                        ;; failure messages.
                        (got-lines
                         (string-append newline-char
                                        prefix "  got: " newline-char
                                        prefix "    expr0: x"
                                        newline-char
                                        (if (string= which "assert")
                                            ""
                                            (string-append
                                             prefix "    expr1: y"
                                             newline-char))))
                        (got-eval-lines
                         (string-append got-lines
                                        prefix "  evaluated: "
                                        newline-char
                                        prefix "    arg0: "
                                        (my-format "~a" x)
                                        newline-char
                                        (if (string= which "assert")
                                            ""
                                            (string-append
                                             prefix "    arg1: "
                                             (my-format "~a" y)
                                             newline-char))))
                        ;; Generate what failure messages will say was
                        ;; not evaluated (#t, which, or approx message).
                        (eval-to (cond ((string= which "assert") "#t")
                                       ((string= which "approximate")
                                        (string-append
                                         "approximately equal (within "
                                         (number->string tol)
                                         " of each other)"))
                                       (else which)))
                        (skip (string= state "SKIP"))
                        (xfail (or (string= state "XFAIL")
                                   (string= state "XPASS"))))
                   (cond ((string= which "assert")
                          (test-assert x name 'skip skip 'xfail xfail))
                         ((string= which "eq?")
                          (test-eq x y name 'skip skip 'xfail xfail))
                         ((string= which "eqv?")
                          (test-eqv x y name 'skip skip 'xfail xfail))
                         ((string= which "equal?")
                          (test-equal x y name 'skip skip 'xfail xfail))
                         (else (test-approximate x y tol name
                                                 'skip skip 'xfail xfail)))
                   ;; Append the output for the respective state.
                   (set! output
                     (cond ((string= state "PASS")
                            (string-append output "ok "
                                           (number->string new-total)
                                           " - " name newline-char))
                           ((string= state "SKIP")
                            (string-append output "ok "
                                           (number->string new-total)
                                           " - " name " # SKIP" newline-char))
                           ((string= state "FAIL")
                            (string-append output "not ok "
                                           (number->string new-total)
                                           " - " name newline-char
                                           prefix "  ---" newline-char
                                           prefix "  message: "
                                           "Arguments did not evaluate "
                                           eval-to
                                           got-eval-lines
                                           prefix "  ..." newline-char))
                           ((string= state "XFAIL")
                            (string-append output "not ok "
                                           (number->string new-total)
                                           " - " name " # TODO" newline-char
                                           prefix "  ---" newline-char
                                           prefix "  message: "
                                           "Arguments did not evaluate "
                                           eval-to
                                           got-eval-lines
                                           prefix "  ..." newline-char))
                           ((string= state "XPASS")
                            (string-append output "ok "
                                           (number->string new-total)
                                           " - " name " # TODO" newline-char
                                           prefix "  ---" newline-char
                                           prefix "  message: "
                                           "Expected to fail but didn't"
                                           got-lines
                                           prefix "  ..." newline-char))
                           (else
                            (string-append output "not ok "
                                           (number->string new-total)
                                           " - " name newline-char
                                           prefix "  ---" newline-char
                                           prefix "  message: "
                                           "Error thrown evaluating "
                                           "expressions" newline-char
                                           prefix "  error: a" newline-char
                                           got-lines
                                           prefix "  ..." newline-char))))
                   ;; Compare output and return diagnostic information
                   ;; if it fails
                   (cond ((not (string= output (get-output)))
                          (begin
                            (set! msg (list
                                       (string-append "  FAILED "
                                                      (number->string count))
                                       "reason: output not correct"
                                       (string-append "  arg0: "
                                                      (my-format "~a" x))
                                       (my-format "~s" output)
                                       (my-format "~s"
                                                  (get-output))))
                            #f))
                         ;; Check the counts and return diagnostic
                         ;; information if it fails
                         ((not (apply check-test-counts new-counts))
                          (begin
                            (set! msg (list
                                       (string-append "  FAILED "
                                                      (number->string count))
                                       "reason: counts not right"
                                       (string-append "  arg0: "
                                                      (my-format "~a" x))
                                       (string-append
                                        "  counts: "
                                        (my-format "~a" (get-test-counts)))
                                       (string-append
                                        "  expected-counts: "
                                        (my-format "~a" new-counts))))
                            #f))
                         (else (loop (+ 1 count) (cdr items0)
                                     (cdr items1)))))))))
         ;; print all messages.
         (for-each (lambda (x) (display (string-append x newline-char)))
                   msg)
         (close-port p))))))

;;; Macro to check all test-assert states. It takes expression lists
;;; that should pass as well as those that should fail. For assert, the
;;; first of each will be used twice. For skip, pass and fail lists
;;; are appended together.
(define-syntax check-asserts
  (syntax-rules ()
    ((check-asserts which tol pexprs0 pexprs1 fexprs0 fexprs1)
     (if (string= which "assert")
         (begin
           (check-asserts-state which "PASS" "Vviene3" "#v" tol
                                pexprs0 pexprs0)
           (check-asserts-state which "FAIL" "38Vau#" "3A" tol
                                fexprs0 fexprs0)
           (check-asserts-state which "XFAIL" "$!v" "38v6*" tol
                                fexprs0 fexprs0)
           (check-asserts-state which "XPASS" "vneAva7$" "E" tol
                                pexprs0 pexprs1)
           (check-asserts-state which "SKIP" "vu A" "VA" tol
                                    (list (list . pexprs0) . fexprs0)
                                    (list (list . pexprs0) . fexprs0))
           (check-asserts-state which "EVAL EXCEPTION" "38va[" "()" tol
                                pexprs0 pexprs1))
         (begin
           (check-asserts-state which "PASS" "Vviene3" "#v" tol
                                pexprs0 pexprs1)
           (check-asserts-state which "FAIL" "38Vau#" "3A" tol
                                fexprs0 fexprs1)
           (check-asserts-state which "XFAIL" "$!v" "38v6*" tol
                                fexprs0 fexprs1)
           (check-asserts-state which "XPASS" "vneAva7$" "E" tol
                                pexprs0 pexprs1)
           (check-asserts-state which "SKIP" "vu A" "VA" tol
                                (list (list . pexprs0) . fexprs0)
                                (list (list . pexprs1) . fexprs1)))))))

;;; Check that test-assert works properly
(check-asserts "assert" 0
               (#t 3 2.1 "3vai" (+ 2 3) (list 3 #f))
               ()
               (#f (= 2 3))
               ())

;;; Make some lists for checking test-eq/eqv/equal
(define a (list 1 3 #f 'avie '(+ 3 4) "va" '(1 2 "b")))
(define b (list 1 3 #f 'avie '(+ 3 4) "va" '(1 2 "b")))
(define c (list 4 2 #t 'ae '(- 3 4) "3v" '(1 2 "b")))

;;; Check that test-eq works properly
(check-asserts "eq?" 0
               (a b c 1 #t #f)
               (a b c 1 #t #f)
               (a b c 1 "a" 2.2)
               (b a a 1.0 "a" 2.2))

;;; Check that test-eqv works properly
(check-asserts "eqv?" 0
               (a b c 1 #t #f 2.2)
               (a b c 1 #t #f 2.2)
               (a b c "a" 3)
               (b a a "a" 3.0))

;;; Check that test-equal works properly
(check-asserts "equal?" 0
               (a b c 1 #t #f 2.2 a "a")
               (a b c 1 #t #f 2.2 b "a")
               (c b 3)
               (a c 3.0))

;;; Check that test-approximate works properly. Different tolerances will
;;; be used.
(check-asserts-state "approximate" "PASS" "vi3ne" "v83"
                     0.1
                     (0 1 2 3 4 5 6 7 8.0 9.0 10.0)
                     (0.01 0.91 2.03 3.09 3.9001 5.099 6.01 7
                           7.98 9.03 10.09))
(check-asserts-state "approximate" "FAIL" "vi3ne" "v83"
                     0.4
                     (0 1 2 3)
                     (1 0.59 2.401 4))
(check-asserts-state "approximate" "XFAIL" "vi3ne" "v83"
                     0.4
                     (0 1 2 3)
                     (1 0.59 2.401 4))
(check-asserts-state "approximate" "XPASS" "vi3ne" "v83"
                     0.1
                     (0 1 2 3 4 5 6 7 8.0 9.0 10.0)
                     (0.01 0.91 2.03 3.09 3.9001 5.099 6.01 7
                           7.98 9.03 10.09))
(check-asserts-state "approximate" "SKIP" "vi3ne" "v83"
                     10
                     (0 10 -103 50)
                     (0 100 -103 90))


;;; Check that test-pred works correctly. It should if all the
;;; previous tests worked but it should be checked. Only thing that
;;; really needs to be checked is 0 and more than 2 arguments as
;;; well as exeptions.

;;; 0 args
(display (response (get-count) "test-pred 0 args"
                   (string= (string-append (TAP-header 1)
                                           "ok 1" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-pred ((lambda () #t))))))))

;;; 3 args
(display (response (get-count) "test-pred 3 args"
                   (string= (string-append (TAP-header 1)
                                           "ok 1" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-pred ((lambda (arg0 arg1 arg2)
                                              (+ arg0 arg1 arg2))
                                            1 2 3)))))))

;;; 6 args
(display (response (get-count) "test-pred 6 args"
                   (string= (string-append (TAP-header 1)
                                           "ok 1" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-pred ((lambda (arg0 arg1 arg2 arg3 arg4 arg5)
                                              (* arg0 arg1 arg2 arg3 arg4 arg5))
                                            1 2 3 0 -1 2.2)))))))

;;; Eval exception
(display (response (get-count) "test-pred EVAL EXCEPTION"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1" newline-char
                                           "  ---" newline-char
                                           "  message: Error thrown "
                                           "evaluating expressions"
                                           newline-char
                                           "  error: blah" newline-char
                                           "  got: " newline-char
                                           "    expr0: 1" newline-char
                                           "    expr1: 2" newline-char
                                           "    expr2: 3" newline-char
                                           "    expr3: (excpt-raise (quote blah))"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-pred ((lambda (arg0 arg1 arg2 arg3)
                                              (* arg0 arg1 arg2 arg3))
                                            1 2 3 (excpt-raise 'blah))))))))

;;; Predicate exception
(display (response (get-count) "test-pred PRED EXCEPTION"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1" newline-char
                                           "  ---" newline-char
                                           "  message: Error thrown "
                                           "applying excpt-raise"
                                           newline-char
                                           "  error: en" newline-char
                                           "  got: " newline-char
                                           "    expr0: (quote en)"
                                           newline-char
                                           "  evaluated: " newline-char
                                           "    arg0: en" newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-pred (excpt-raise 'en)))))))


;;; Check test-error

;;; Catch an error when catching all errors (PASS)
(display (response (get-count) "test-error catch all PASS"
                   (string= (string-append (TAP-header 1)
                                           "ok 1" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error #t (+ 1 "a")))))))

;;; Catch no error when catching all errors (FAIL)
(display (response (get-count) "test-error catch all FAIL"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1" newline-char
                                           "  ---" newline-char
                                           "  message: "
                                           "Exception was not thrown"
                                           newline-char
                                           "  error: #t" newline-char
                                           "  got: " newline-char
                                           "    expr0: (+ 1 2)"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error #t (+ 1 2)))))))

;;; Catch no error when catching all errors (XFAIL)
(display (response (get-count) "test-error catch all XFAIL"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1 # TODO"
                                           newline-char
                                           "  ---" newline-char
                                           "  message: "
                                           "Exception was not thrown"
                                           newline-char
                                           "  error: #t" newline-char
                                           "  got: " newline-char
                                           "    expr0: (+ 1 2)"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error #t (+ 1 2) 'xfail #t))))))

;;; Catch an error when catching all errors (XPASS)
(display (response (get-count) "test-error catch all XPASS"
                   (string= (string-append (TAP-header 1)
                                           "ok 1 # TODO"
                                           newline-char
                                           "  ---" newline-char
                                           "  message: "
                                           "Expected to fail but didn't"
                                           newline-char
                                           "  got: " newline-char
                                           "    expr0: (+ 1 a)"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error #t (+ 1 "a") 'xfail #t))))))

;;; Skip catch no error when catching all errors (SKIP)
(display (response (get-count) "test-error catch all SKIP"
                   (string= (string-append (TAP-header 1)
                                           "ok 1 # SKIP" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error #t (+ 1 2) 'skip #t))))))


;;; Catch a specific error (PASS)
(display (response (get-count) "test-error specific error PASS"
                   (string= (string-append (TAP-header 1)
                                           "ok 1" newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error 'ab (excpt-raise 'ab)))))))

;;; Fail to catch anything while trying to catch a specific error (FAIL)
(display (response (get-count) "test-error specific error none thrown FAIL"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1" newline-char
                                           "  ---" newline-char
                                           "  message: "
                                           "Exception was not thrown"
                                           newline-char
                                           "  error: ab" newline-char
                                           "  got: " newline-char
                                           "    expr0: (+ 1 2)"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error 'ab (+ 1 2)))))))

;;; Catch wrong error while trying to catch a specific error (FAIL)
(display (response (get-count) "test-error specific error wrong thrown FAIL"
                   (string= (string-append (TAP-header 1)
                                           "not ok 1" newline-char
                                           "  ---" newline-char
                                           "  message: "
                                           "Wrong exception was thrown"
                                           newline-char
                                           "  error: " newline-char
                                           "    got: ef" newline-char
                                           "    expected: ab" newline-char
                                           "  got: " newline-char
                                           "    expr0: "
                                           "(excpt-raise (quote ef))"
                                           newline-char
                                           "  ..." newline-char)
                            (call-with-string-output-port
                              (lambda (p)
                                (test-begin 1 'port p)
                                (test-error 'ab (excpt-raise 'ef)))))))


;;; Test group features.

;;; Enter and leave group without any tests.
(display (response (get-count) "group no tests"
                   (let ((group-name "aivi38")
                         (name "34(3")
                         (group-success #f)
                         (output-same #t))
                     (set! output-same
                       (string=? (string-append (TAP-header 1)
                                                "ok 1 - " group-name
                                                newline-char)
                                 (call-with-string-output-port
                                  (lambda (p)
                                    (test-begin 1 'port p)
                                    (test-group-begin group-name)
                                    (set! group-success
                                      (string=? group-name
                                                *test-group-name*))
                                    (test-group-end)))))
                     (and output-same group-success
                          (not *test-group-failed*)
                          (check-test-counts 1 0 0 0 0)))))

;;; Enter and leave group with two passed tests
(display (response (get-count) "group with two PASS tests"
                   (let ((group-name "[{e8")
                         (name "338V'")
                         (group-success #f)
                         (output-same #t))
                     (set! output-same
                       (string=? (string-append (TAP-header 1)
                                                "ok 1 - " group-name
                                                newline-char)
                                 (call-with-string-output-port
                                  (lambda (p)
                                    (test-begin 1 'port p)
                                    (test-group-begin group-name)
                                    (test-assert #t name)
                                    (test-assert #t name)
                                    (set! group-success
                                      (string=? group-name
                                                *test-group-name*))
                                    (test-group-end)))))
                     (and output-same group-success
                          (not *test-group-failed*)
                          (check-test-counts 1 0 0 0 0)))))

;;; Enter and leave group with two tests (XFAIL PASS)
(display (response (get-count) "group with XFAIL PASS tests"
                   (let ((group-name "38vn#")
                         (name "znvu$a8g")
                         (group-success #f)
                         (output-same #t))
                     (set! output-same
                       (string=? (string-append (TAP-header 1)
                                                "ok 1 - " group-name
                                                newline-char)
                                 (call-with-string-output-port
                                  (lambda (p)
                                    (test-begin 1 'port p)
                                    (test-group-begin group-name)
                                    (test-assert #f name 'xfail #t)
                                    (test-assert #t name)
                                    (set! group-success
                                      (string=? group-name
                                                *test-group-name*))
                                    (test-group-end)))))
                     (and output-same group-success
                          (not *test-group-failed*)
                          (check-test-counts 1 0 0 0 0)))))

;;; Enter and leave group with two tests (FAIL PASS)
(display (response (get-count) "group with FAIL PASS tests"
                   (let ((group-name "v83nA")
                         (name "i$@#fa'")
                         (group-success #f)
                         (output-same #t))
                     (set! output-same
                       (string=? (string-append (TAP-header 1)
                                                "not ok 1 - " group-name
                                                "->" name
                                                newline-char
                                                "  ---" newline-char
                                                "  message: Arguments did "
                                                "not evaluate #t"
                                                newline-char
                                                "  got: " newline-char
                                                "    expr0: #f" newline-char
                                                "  evaluated: " newline-char
                                                "    arg0: #f" newline-char
                                                "  ..." newline-char)
                                 (call-with-string-output-port
                                  (lambda (p)
                                    (test-begin 1 'port p)
                                    (test-group-begin group-name)
                                    (test-assert #f name)
                                    (test-assert #t name)
                                    (set! group-success
                                      (string=? group-name
                                                *test-group-name*))
                                    (test-group-end)))))
                     (and output-same group-success
                          *test-group-failed*
                          (check-test-counts 0 1 0 0 0)))))

;;; Enter and leave group with two tests (XPASS PASS)
(display (response (get-count) "group with XPASS PASS tests"
                   (let ((group-name "v83Avn")
                         (name "[Nviea'")
                         (group-success #f)
                         (output-same #t))
                     (set! output-same
                       (string=? (string-append (TAP-header 1)
                                                "ok 1 - " group-name
                                                "->" name
                                                " # TODO"
                                                newline-char
                                                "  ---" newline-char
                                                "  message: Expected to "
                                                "fail but didn't"
                                                newline-char
                                                "  got: " newline-char
                                                "    expr0: #t" newline-char
                                                "  ..." newline-char)
                                 (call-with-string-output-port
                                  (lambda (p)
                                    (test-begin 1 'port p)
                                    (test-group-begin group-name)
                                    (test-assert #t name 'xfail #t)
                                    (test-assert #t name)
                                    (set! group-success
                                      (string=? group-name
                                                *test-group-name*))
                                    (test-group-end)))))
                     (and output-same group-success
                          *test-group-failed*
                          (check-test-counts 0 0 0 1 0)))))

;;; Double begin group
(display (response (get-count) "group ERROR begin twice"
                   (let ((group-name "v83Avn")
                         (had-error #f))
                     (call-with-string-output-port
                      (lambda (p)
                        (excpt-guard (key (#t (set! had-error #t)))
                                     (test-begin 1 'port p)
                                     (test-group-begin group-name)
                                     (test-group-begin group-name)
                                     (set! had-error #f))))
                     had-error)))

;;; End unstarted group
(display (response (get-count) "group ERROR end group not started"
                   (let ((group-name "vv93n")
                         (had-error #f))
                     (call-with-string-output-port
                      (lambda (p)
                        (excpt-guard (key (#t (set! had-error #t)))
                                     (test-begin 1 'port p)
                                     (test-group-end)
                                     (set! had-error #f))))
                     had-error)))


;;; Check test-group-with-cleanup

;;; test-group-with-cleanup everything ok
(display (response (get-count) "test-group-with-cleanup no errors"
                   (let ((finished #f)
                         (cleaned-up #f))
                     (call-with-string-output-port
                      (lambda (p)
                        (test-begin 1 'port p)
                        (test-group-with-cleanup "avu3n"
                          (begin
                            (test-assert #t)
                            (test-assert #f)
                            (set! finished #t))
                          (set! cleaned-up #t))))
                     (and finished cleaned-up
                          (string-null? *test-group-name*)))))

;;; test-group-with-cleanup error in body
(display (response (get-count)
                   "test-group-with-cleanup error in body"
                   (let ((finished #f)
                         (cleaned-up #f))
                     (call-with-string-output-port
                      (lambda (p)
                        (test-begin 1 'port p)
                        (test-group-with-cleanup "avu3n"
                          (begin
                            (test-assert #t)
                            (test-assert #f)
                            (excpt-raise 'ae)
                            (set! finished #t))
                          (set! cleaned-up #t))))
                     (and (not finished) cleaned-up
                          (string-null? *test-group-name*)))))
