;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-STARTUP; Base: 10; Lowercase: Yes -*-
;;;
;;; initialization.lisp --- Initialization methods
;;;
;;; Time-stamp: <Friday Dec  9, 2011 07:26:43 asmodai>
;;; Revision:   13
;;;
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Dec 2011 05:45:07
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
(error "This will probably not work with Genera...")

#-genera
(in-package #:cl-startup)


;;; ==================================================================
;;; {{{ Variables:

(defvar *boot-initialization-list* nil
  "Initializations to be run on a boot.")

(defvar *once-only-initialization-list* nil
  "Initializations to be run once only.")

(defvar *system-initialization-list* nil
  "Initializations to be run before a boot.")

(defvar *user-application-initialization-list* nil
  "Initializations for user applications.")

;;;
;;; Keywords:
;;;
;;;  NOW               Run the init now.
;;;  FIRST             Run the init now if this is the first entry for
;;;                    the  specified name.
;;;  NORMAL            Do the `normal' thing (init when
;;;                    initializations are normally run.)
;;;  REDO              Do nothing now, but set things up so init gets
;;;                    redone.
;;;  BOOT              Use the cold boot list.
;;;  USER-APPLICATION  Use the user application list.
;;;  ONCE              Use the once-only list.
;;;  SYSTEM            Use the system list.
;;;  SITE              Use the site list (also run once.)
;;;  SITE-OPTION       Use the site-option list (also run once.)
;;;  HEAD-OF-LIST      If the entry is not presently on the list, add
;;;                    it to the front instead of the end of the list.
;;;
(defvar *initialization-keywords*
  '((site             *site-initialization-list*)
    (site-option      *site-option-initialization-list*)
    (system           *system-initialization-list*           first)
    (once             *once-only-initialization-list*        first)
    (user-application *user-application-initialization-list*)
    (boot             *boot-initialization-list*))
  "Alist defining keywords accepted by ADD-INITIALIZATION.

Each element looks like (KEYWORD LIST-VARIABLE-NAME [TIME-TO-RUN]).
TIME-TO-RUN should be one of NOW, FIRST, NORMAL, REDO, or omitted.

It is a default in case the ADD-INITIALIZATION doesn't specify any of
them.")

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Structures:

(defstruct (init-list-entry
            (:constructor make-init-list-entry
                          (name form flag))
            (:conc-name "INIT-"))
  name
  form
  flag)

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Functions:

(defmacro init-list-check (name)
  `(progn
     (unless (boundp ,name)
       (set ,name nil))
     (unless (get ,name 'initialization-list)
       (setf (get ,name 'initialization-list) t))))

(defun initializations (list-name &optional (redo-flag nil) (flag t))
  (init-list-check list-name)
  (dolist (init (symbol-value list-name))
    (when (or (null (init-flag init))
              redo-flag)
      (restart-case
          (let* ((old-standard-output *standard-output*)
                 (out-stream (make-string-output-stream))
                 (*standard-output* (make-broadcast-stream
                                     out-stream)))
            (format old-standard-output
                    "~&;; -> Starting ~a <-~%"
                    (init-name init))
            (eval (init-form init))
            (format old-standard-output
                    "~&~A~%"
                    (get-output-stream-string out-stream)))
          (abort-init ()
                      :report "Abort the ~A initialization."
                      (init-name init)))
      (setf (init-flag init) flag))))

(defun add-initialization (name form &optional keywords
                           (list-name '*boot-initialization-list*)
                           &aux when default-when init head-of-list)
  "Add an initialization with name NAME and definition FORM to an
initialization list.

NAME should be a string and FORM an expression to be evaluated later.
KEYWORDS can be one keyword or a list of keywords.  These keywords can
be in any package.

Keywords can either be HEAD-OF-LIST, meaning `add to the front of the
list rather than the end,' BOOT, ONCE, SYSTEM, SITE, or SITE-OPTION,
specifying a list (note that only *one* initialization list name
keyword is allowed in a keyword list), or NOW, FIRST, NORMAL, or REDO,
saying when to run the init.

NOW means run the init as well as adding it to the list; FIRST means
run the init now if it isn't on the list; NORMAL means don't run the
init now; REDO means don't run it now, but mark it as never having
been run even if it is already on the list and has been run.

If the keywords do not specify the list, LIST-NAME is used.

The default initialization list is *BOOT-INITIALIZATION-LIST*"
  (when keywords
    (do ((l (if (listp keywords)
                keywords
                (list keywords))
            (cdr l))
         v
         keydef)
        ((null l))
      (setq v (symbol-name (car l)))
      (if (setq keydef (assoc v *initialization-keywords*
                              :test #'string-equal))
          (setq list-name (cadr keydef)
                default-when (caddr keydef))
          (cond ((member v '("NOW" "FIRST" "NORMAL" "REDO")
                         :test #'string-equal)
                 (setq when v))
                ((string-equal "HEAD-OF-LIST" v)
                 (setq head-of-list t))
                (t
                 (error "Illegal initialization keyword ~S"
                        (car l)))))))
  (setq when
        (case (or when default-when)
          ("NORMAL"  nil)
          ("NIL"     nil)
          ("NOW"     'now)
          ("REDO"    'redo)
          ("FIRST"   'first)))
  (init-list-check list-name)
  (setq init
        (do ((l (symbol-value list-name)
                (cdr l)))
            ((null l)
             (cond ((or head-of-list
                        (null (symbol-value list-name)))
                    (car (set list-name
                              (cons (make-init-list-entry
                                     name
                                     form
                                     nil)
                                    (symbol-value list-name)))))
                   (t
                    (cadr (rplacd (last (symbol-value list-name))
                                  (list (make-init-list-entry
                                         name
                                         form
                                         nil)))))))
          (when (string-equal (init-name (car l)) name)
            (setf (init-form (car l)) form)
            (return (car l)))))
  (cond ((eq when 'redo)
         (setf (init-flag init) nil))
        ((or (eq when 'now)
             (and (eq when 'first)
                  (null (init-flag init))))
         (eval (init-form init))
         (setf (init-flag init) t))))

(defun delete-initialization (name &optional keywords
                              (list-name '*boot-initialization-list*))
  "Remove any initialization named NAME from an initialization list.

NAME should be a string.  KEYWORDS can be a keyword or a list of them;
packages do not matter.  The only thing you can specify with one is
what list to remove from.  Or let KEYWORDS be NIL and supply the list
name symbol as LIST-NAME."
  (do ((l (if (listp keywords)
              keywords
              (list keywords))
          (cdr l))
       keydef
       v)
      ((null l))
    (setq v (symbol-name (car l)))
    (if (setq keydef (assoc v *initialization-keywords*
                            :test #'string-equal))
        (setq list-name (cadr keydef))
        (error "Illegal keyword ~S." (car l))))
  (init-list-check list-name)
  (do ((l (symbol-value list-name) (cdr l))
       (flag nil))
      ((null l) flag)
    (when (string-equal (init-name (car l)) name)
      (set list-name (delete (car l)
                             (symbol-value list-name)
                             :test #'eq))
      (setq flag t))))

(defun reset-initializations (list-name)
  "Mark all the inits in the initialization list named LIST-NAME as
not yet run."
  (init-list-check list-name)
  (do ((l (symbol-value list-name) (cdr l)))
      ((null l))
    (setf (init-flag (car l)) nil)))

(defun initialize-system ()
  (cmsg "STARTUP PROCEDURE~%")
  (cmsg "Initializing boot:")
  (initializations '*boot-initialization-list*)
  (cmsg "Finished boot.~%")
  
  (cmsg "Initializing system:")
  (initializations '*system-initialization-list* t)
  (cmsg "Finished system.~%")

  (cmsg "Initializing user applications:")
  (initializations '*user-application-initialization-list*)
  (cmsg "Finished user applications.~%")
  
  (cmsg "Startup complete.")
  (values))
  
;;; }}}
;;; ==================================================================

;;; initialization.lisp ends here
