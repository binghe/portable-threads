(in-package :cl-user)

(defpackage #:portable-threads.system
  (:use #:cl #:asdf))

(in-package #:portable-threads.system)

(defsystem portable-threads
  :author "Dan Corkill <corkill@GBBopen.org>"
  :maintainer "Chun Tian <binghe.lisp@gmail.com"
  :licence "Part of the GBBopen Project (see LICENSE for license information)."
  :description "Portable Threads."
  :components ((:static-file "COPYING")
               (:static-file "LICENSE")
               (:file "portable-threads")
               (:file "scheduled-periodic-functions" :depends-on "portable-threads")))
