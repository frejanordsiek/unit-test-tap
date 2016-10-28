;;; tests-r7rs-header.scm --- R7RS test header for the unit-test-tap module
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


(import (except (scheme base) guard raise))
(import (prefix (only (scheme base) guard raise) excpt-))
(import (scheme write))

;; Make the R6RS call-with-string-output-port and
;; open-string-output-port in terms of R7RS
;; open-output-string and get-output-string.
(define open-string-output-port
  (lambda ()
    (let ((p (open-output-string)))
      (values p (lambda () (get-output-string p))))))

(define call-with-string-output-port
  (lambda (proc)
    (let ((p (open-output-string)))
      (proc p)
      (let ((out-s (get-output-string p)))
        (close-port p)
        out-s))))

;; Make R6RS cons*
(define-syntax cons*
  (syntax-rules ()
    ((cons*) '())
    ((cons* arg0) arg0)
    ((cons* arg0 arg1 . args) (cons arg0 (cons* arg1 . args)))))

;; Make R6RS for-all
(define-syntax for-all
  (syntax-rules ()
    ((for-all proc . args)
     (call-with-current-continuation
      (lambda (return)
        (map (lambda ( arg0 . other-args)
               (if (if (= 0 (length other-args))
                       (proc arg0)
                       (apply proc (append (list arg0) other-args)))
                   #t
                   (return #f))) . args)
        (return #t))))))
