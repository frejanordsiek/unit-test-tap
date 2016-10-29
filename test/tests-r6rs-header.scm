;;; tests-r6rs-header.scm --- R6RS test header for the unit-test-tap module
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


(import (rnrs base (6))
        (rnrs lists (6))
        (prefix (rnrs exceptions (6)) excpt-)
        (rnrs io ports (6))
        (rnrs io simple (6))
        (unit-test-tap))

;;; Make a non-destructive version of open-string-output-port. The R6RS
;;; standard says the read procedure it returns is destructive (all text
;;; in the string port is cleared), but it is preferable to have a
;;; non-destructive version.
(define open-string-output-port-nondestructive
  (lambda ()
    (let-values (((p get-output) (open-string-output-port)))
      (values p
              ;; Read the output two times and compare them. If they
              ;; are not equal, then the read was destructive as R6RS
              ;; instructs and the data needs to be written back.
              ;; Otherwise, the scheme implementation doesn't quite
              ;; follow this behavior and the output can be left as is.
              (lambda () (let ((out (get-output))
                               (out2 (get-output)))
                           (if (not (string=? out out2))
                               (begin
                                 (put-string p out)
                                 (flush-output-port p)))
                           out))))))
