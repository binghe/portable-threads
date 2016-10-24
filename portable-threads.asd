;;;; -*- Mode:Common-Lisp; Package:Portable-Threads-System; Syntax:common-lisp -*-
;;;; *-* File: /usr/local/gbbopen/portable-threads.asd *-*
;;;; *-* Edited-By: cork *-*
;;;; *-* Last-Edit: Wed Apr  7 10:24:26 2010 *-*
;;;; *-* Machine: cyclone.cs.umass.edu *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                 Stand-Alone ASDF for Portable Threads 
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Attila Lendvai
;;;
;;; Part of the GBBopen Project.
;;; Licensed under Apache License 2.0 (see LICENSE for license information).
;;;
;;;  Note: This ASDF system definition is for stand-alone Portable Threads
;;;        use.  The current gbbopen.asd interface does not play nice with
;;;        this one.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  11-15-07 File created.  (Lendvai)
;;;  11-23-07 Replaced non-portable :pathnames with :modules.  (Costanza)
;;;  10-24-16 Added scheduled-periodic-functions.lisp into this system
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

(require :asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :portable-threads-system)
    (defpackage :portable-threads-system
      (:use :common-lisp :asdf))))

(in-package :portable-threads-system)

;;; ---------------------------------------------------------------------------

(defsystem :portable-threads
    :version "2.3"
    :author "The GBBopen Project <gbbopen@GBBopen.org>"
    :maintainer "Dan Corkill <corkill@GBBopen.org> & Chun Tian (binghe) <binghe.lisp@gmail.com>"
    :licence 
      "Part of the GBBopen Project. Licensed under Apache License 2.0 (see LICENSE for license information)."
    :description "Portable Threads"
    :components ((:static-file "COPYING")
                 (:static-file "LICENSE")
                 (:file "portable-threads")
                 (:file "scheduled-periodic-functions" :depends-on ("portable-threads"))))

;;; ---------------------------------------------------------------------------

(defsystem :portable-threads-test
    :depends-on (:portable-threads)
    :components ((:module
                  "test"
                  :components
                  ((:file "portable-threads-test")))))

;;; ---------------------------------------------------------------------------

(defmethod perform ((op test-op)
                    (system (eql (find-system :portable-threads))))
  (operate 'load-op ':portable-threads-test)
  (funcall (intern (symbol-name '#:portable-threads-tests) 
                   :portable-threads-user))
  (values))

(defmethod operation-done-p ((op test-op) 
                             (system (eql (find-system :portable-threads))))
  nil)

;;; ===========================================================================
;;;				  End of File
;;; ===========================================================================
