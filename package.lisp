;;;; package.lisp

(defpackage #:userfig
  (:use #:cl #:alexandria #:cl-who)
  (:export
   #:get-user-data
   #:initialize-user
   #:get-user-visible-data
   #:update-from-user
   #:userfig-component))

