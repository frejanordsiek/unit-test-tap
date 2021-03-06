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

(define-library (unit-test-tap)
  (export test-port test-yaml-prefix test-count test-number
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
          test-approximate test-error)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (begin
    ;; Make the R6RS call-with-string-output-port
    ;; in terms of R7RS open-output-string and get-output-string.
    (define call-with-string-output-port
      (lambda (proc)
        (let ((p (open-output-string)))
          (proc p)
          (flush-output-port p)
          (let ((out-s (get-output-string p)))
            (close-port p)
            out-s))))
    ;; Make R6RS cons*
    (define-syntax cons*
      (syntax-rules ()
        ((cons*) '())
        ((cons* arg0) arg0)
        ((cons* arg0 arg1 . args) (cons arg0 (cons* arg1 . args)))))


