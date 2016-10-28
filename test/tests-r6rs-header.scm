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


(import (rnrs base (6)))
(import (only (rnrs lists (6)) for-all))
(import (prefix (rnrs exceptions (6)) excpt-))
(import (only (rnrs io ports (6))
              call-with-string-output-port
              open-string-output-port))
(import (rnrs io simple (6)))

