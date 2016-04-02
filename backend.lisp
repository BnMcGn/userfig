(in-package :userfig)

#|

Fieldspec:
Shall consist of one or more symbol "keys" designating the field name followed
by a plist of options for the field.

Options are:
:initial - Default starting value, field undefined if not specified.
:viewable - t, nil, a symbol or a list of symbols. Symbols are roles for which
            viewable is true.
:editable - t, nil, a symbol or a list of symbols. Symbols are roles for which
            editable is true.
:type - Typespec
:description - One line, brief description of field
:documentation - More in-depth usage information.

Typespec is one of:
  - :string (default)
  - :integer
  - :boolean
  - :datestamp
  - (:pickone ...)
  - (:picksome ...)

FIXME: What about cases where someone wants to set (:somekey :subkey) to a value
 but also set :somekey to a value. (value :somekey) should end up being a key
store of some sort - perhaps a hash table - so it can't be an arbitrary value.


|#



(defun normalize-fieldspec (fspec)
  (let ((names (butlast fspec))
        (fspec (car (last fspec))))
    (unless (every #'symbolp names)
      (error "Field names must be symbols"))
    (values
     names
     (list
      :initial (getf fspec :initial)
      :viewable (getf fspec :viewable t)
      :editable (getf fspec :editable t)
      :type (validate-typespec (getf fspec :type :string))
      :description (getf fspec :description "")
      :documentation (getf fspec :documentation "")))))

(defun validate-typespec (tspec)
  (or (member tspec '(:string :integer :boolean :datestamp))
      (and (listp tspec)
           (member (car tspec) '(:pickone :picksome)))))

(defmacro do-fieldspecs ((names spec source) &body body)
  (with-gensyms (src rest curr)
    `(loop with ,src = ,source
        while ,src
        do
          (multiple-value-bind (,curr ,rest)
              (gadgets:divide-list+ ,src #'listp)
            (setf ,src ,rest)
            (let ((,names (butlast ,curr))
                  (,spec (car (last ,curr))))
              ,@body)))))

(defpackage #:userfig.usernames)
(defparameter *username-package* 'userfig.usernames)

;;;;;;
;;; Interaction with ubiquitous
;;;;;;

(defun restore-user (username)
  (ubiquitous:restore (intern username (find-package *username-package*))))

(eval-always
  (defmacro with-user (username &body body)
   `(progn
      (restore-user ,username)
      ,@body)))

(defun get-user-data (username fieldspecs)
  (ubiquitous:with-transaction ()
    (restore-user username)
    (cl-hash-util:collecting-hash-table (:mode :replace :test #'equal)
      (do-fieldspecs (names tspec fieldspecs)
        (cl-hash-util:collect names
          (gadgets:aif2only
           (apply #'ubiquitous:value names)
           anaphora:it
           (getf tspec :initial)))))))

(defun set-user-data (username &rest key/s-and-values)
  (ubiquitous:with-transaction ()
    (restore-user username)
    (gadgets:map-by-2
     (lambda (keys value)
       (setf (apply #'ubiquitous:value (ensure-list keys)) value))
     key/s-and-values)))

