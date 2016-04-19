;;;; package.lisp

(defpackage #:userfig
  (:use #:cl #:alexandria)
  (:export
   #:get-user-data
   #:initialize-user
   #:get-user-visible-data
   #:update-from-user
   #:userfig-component))

