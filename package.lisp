;;;; package.lisp

(defpackage #:userfig
  (:use #:cl #:alexandria #:cl-who #:parenscript #:cl-react)
  (:shadowing-import-from #:parenscript #:switch)
  (:export
   #:get-user-data
   #:initialize-user
   #:get-user-visible-data
   #:update-from-user
   #:userfig-component))

