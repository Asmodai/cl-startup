;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; package.lisp --- Package definition.
;;;
;;; Time-stamp: <Friday Dec  9, 2011 05:44:31 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 05:42:38
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
;;; {{{ Commentary:
;;;
;;; }}}

#+genera
(error "This probably won't work with Genera...")

#-genera
(in-package #:cl-user)

(defpackage #:cl-startup
  (:use :common-lisp
        :cl-hacks)
  (:export #:*boot-initialization-list*
           #:*once-only-initialization-list*
           #:*system-initialization-list*
           #:*user-application-initialization-list*
           #:init-list-entry
           #:initializations
           #:*initialization-keywords*
           #:add-initialization
           #:delete-initialization
           #:reset-initializations
           #:initialize-system))

;;; package.lisp ends here

