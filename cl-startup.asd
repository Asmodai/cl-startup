;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; cl-startup.asd --- ASDF system definition
;;;
;;; Time-stamp: <Friday Dec  9, 2011 06:52:36 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 05:48:53
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;; }}}
;;;
;;; {{{ Commentary:
;;;
;;; }}}

#+genera
(error "Please do not load this file into a Symbolics system. ~@
        This is only for Common Lisp systems that support ASDF.")

(in-package #:common-lisp-user)

(defpackage cl-startup-system
  (:use #:asdf
        #:common-lisp))

(in-package #:cl-startup-system)

(defsystem cl-startup
    :name "ASDF system definition"
    :author "Paul Ward <asmodai@gmail.com>"
    :version "1.0"
    :maintainer "Paul Ward <asmodai@gmail.com>"
    :license "Lisp Lesser General Public License (LLGPL)"
    :description "<fill this in>"
    :long-description "<fill this in>"

    :depends-on (:cl-hacks)

    :components
    ((:file "package")
     (:file "initialization" :depends-on ("package"))))

;;; cl-startup.asd ends here

