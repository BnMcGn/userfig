(in-package :cl-user)
(defpackage userfig-test
    (:use :cl :prove :userfig))
(in-package :userfig-test)

(defparameter *fields-raw*
  '(:email
    (:email
     :description "Your email address")
    :favorite :day-of-month
    ((:pickone 1 3 18)
     :description "Favorite day of the month"
     :documentation "Other days are available by special request")
    :system :watch-level
    (:integer
     :initial 4
     :viewable nil
     :editable nil
     :description "Numerical paranoia level indicator"
     :documentation "0 - ignore, 1 - ignore aggressively, 2 or higher - watch obsessively! discover user's hidden cookie supply! profit!")))

(defparameter *fields* (userfig::validate-fieldspecs *fields-raw*))

(defparameter *uname* "hilee-uNprubabbl-Yooser-nAm12333")

(defparameter *test-env1*
  (list
  :LACK.SESSION
  (CL-HASH-UTIL:ALIST->HASH
   `((:USERNAME . ,*uname*))
   :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
  :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO
  "/userfig/get-user-info"
  :SERVER-PROTOCOL :HTTP/1.1 :REQUEST-URI
  "/userfig/get-user-info" :URL-SCHEME "http" :REMOTE-ADDR "127.0.0.1"
  :BODY-PARAMETERS NIL))

(defparameter *test-env2*
  (list
   :LACK.SESSION
   (CL-HASH-UTIL:ALIST->HASH
    `((:USERNAME . ,*uname*))
    :EXISTING (MAKE-HASH-TABLE :TEST #'EQUAL))
   :REQUEST-METHOD :GET :SCRIPT-NAME "" :PATH-INFO "/userfig/settings"
   :SERVER-PROTOCOL :HTTP/1.1 :REQUEST-URI "/userfig/settings"
   :URL-SCHEME "http" :REMOTE-ADDR "127.0.0.1" :REMOTE-PORT 39628
   :QUERY-STRING NIL :CONTENT-TYPE NIL :CLACK.STREAMING T
   :BODY-PARAMETERS NIL))

(plan 15)

(ok (getf (getf *fields* :email) :viewable))
(ok (functionp (getf (getf *fields* :email) :compiled-validator)))

(initialize-user *uname* *fields*)
(setf userfig:*userfig-user* *uname*)

(let ((udata (get-user-data *uname* *fields*)))
  (is 4 (gethash '(:system :watch-level) udata))
  (is nil (gethash '(:email) udata)))

(let ((udata (get-user-visible-data *uname* *fields*)))
  (is-values (gethash '(:system :watch-level) udata) '(nil nil))
  (ok (not (gethash '(:email) udata))))

(let ((indata (make-hash-table :test #'equal)))
  (setf (gethash '(:email) indata) "asdfasdf")
  (is-error (update-from-user *uname* *fields* indata) 'simple-error)
  (setf (gethash '(:email) indata) "asdf@asd.f")
  (ok (update-from-user *uname* *fields* indata))
  (setf (hu:hget/extend indata '(:system :watch-level)) 0)
  (is-error (update-from-user *uname* *fields* indata) 'simple-error))

(let ((udata (get-user-data *uname* *fields*)))
  (is (gethash '(:email) udata) "asdf@asd.f")
  (is 4 (gethash '(:system :watch-level) udata)))

(let ((component (funcall (userfig-component *fields-raw*) nil))
      (result nil))
  (ok (setf result (funcall component *test-env1*)))
  (ok (search "f@a" (car (third result))))
  (ok (setf result (funcall component *test-env2*)))
  (is "text/html" (second (second result))))

(setf userfig:*userfig-user* nil)
(finalize)

