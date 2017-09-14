;;;; package.lisp

(defpackage #:userfig
  (:use #:cl #:alexandria #:cl-who #:parenscript #:cl-react)
  (:shadowing-import-from #:parenscript #:switch)
  (:export
   #:get-user-data
   #:initialize-user
   #:get-user-visible-data
   #:update-from-user
   #:userfig-component
   #:userfig-value
   #:*userfig-user*
   #:map-users
   #:*userfig-realm*
   #:settings-url
   #:new-user-p
   #:initialized?
   #:check-init
   #:with-env
   #:get-user-list
   #:remove-user
   #:userfig-value-for))

