(in-package :cl-user)
(defpackage userfig-test
    (:use :cl :prove :userfig))
(in-package :userfig-test)

(defparameter *fields-raw*
  '(:email
    (:type :email
     :description "Your email address")
    :favorite :day-of-month
    (:type (:pickone 1 3 18)
     :description "Favorite day of the month"
     :documentation "Other days are available by special request")
    :system :watch-level
    (:type :integer
     :initial 4
     :viewable nil
     :editable nil
     :description "Numerical paranoia level indicator"
     :documentation "0 - ignore, 1 - ignore aggressively, 2 or higher - watch obsessively! discover user's hidden cookie supply! profit!")))

(defparameter *fields* (userfig::validate-fieldspecs *fields-raw*))

(defparameter *uname* "hilee-uNprubabbl-Yooser-nAm12333")

(plan 11)

(ok (getf (getf *fields* :email) :viewable))
(ok (functionp (getf (getf *fields* :email) :compiled-validator)))

(initialize-user *uname* *fields*)

(let ((udata (get-user-data *uname* *fields*)))
  (is 4 (gethash '(:system :watch-level) udata))
  (is nil (gethash '(:email) udata)))

(let ((udata (get-user-visible-data *uname* *fields*)))
  (is-values (gethash '(:system :watch-level) udata) '(nil nil))
  (ok (not (gethash '(:email) udata))))

(let ((indata (make-hash-table)))
  (setf (gethash :email indata) "asdfasdf")
  (is-error (update-from-user *uname* *fields* indata) 'simple-error)
  (setf (gethash :email indata) "asdf@asd.f")
  (ok (update-from-user *uname* *fields* indata))
  (setf (hu:hget/extend indata '(:system :watch-level)) 0)
  (is-error (update-from-user *uname* *fields* indata) 'simple-error))

(let ((udata (get-user-data *uname* *fields*)))
  (is (gethash '(:email) udata) "asdf@asd.f")
  (is 4 (gethash '(:system :watch-level) udata)))

(finalize)
