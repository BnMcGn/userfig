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
:widget - A widget specifier. Optional. Widgets can generally be inferred from
  :type.
:config - Options to be passed through to the widget.
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

(gadgets:eval-always (defparameter *userfig-user* nil))

(defun normalize-userfig-fieldspec (fspec)
  (let ((names (butlast fspec))
        (fspec (car (last fspec))))
    (unless (every #'symbolp names)
      (error "Field names must be symbols"))
    (values
     names
     (list*
      :viewable (gadgets:or2 (gadgets:fetch-keyword :viewable fspec) t)
      :editable (gadgets:or2 (gadgets:fetch-keyword :editable fspec) t)
      (webhax-validate:normalize-fieldspec-body fspec)))))

(defmacro do-fieldspecs ((names spec source) &body body)
  (with-gensyms (src rest curr)
    `(loop with ,src = ,source
        while ,src
        do
          (multiple-value-bind (,curr ,rest)
              (gadgets:divide-after-true #'listp ,src)
            (setf ,src ,rest)
            (let ((,names (butlast ,curr))
                  (,spec (car (last ,curr))))
              ,@body)))))

(defun validate-fieldspecs (fieldspecs)
  (gadgets:collecting
    (do-fieldspecs (names spec fieldspecs)
      (multiple-value-bind (nnames nspec)
          (normalize-userfig-fieldspec (concatenate 'list names (list spec)))
        (mapc #'gadgets:collect nnames)
        (gadgets:collect nspec)))))

;;;In case that multiple user namespaces are ever needed:
(defpackage #:userfig.usernames)
(defparameter *username-package* 'userfig.usernames)

;;;;;;
;;; Interaction with ubiquitous
;;;;;;

;;;FIXME: Userfig is going to run into scaling issues because ubiquitous
;;; is, I think, holding all user info in memory. Will need to implement
;;; a db backend for ubiquitous, or work out an alternative.

(defparameter *userfig-realm* 'userfig)

(gadgets:eval-always
  (defmacro with-userfig-restored (&body body)
    `(progn (ubiquitous:restore *userfig-realm*)
            ,@body)))

(defun get-user-data (username fieldspecs)
  (ubiquitous:with-transaction ()
    (with-userfig-restored
      (check-init)
      (cl-hash-util:collecting-hash-table (:mode :replace :test #'equal)
       (do-fieldspecs (names tspec fieldspecs)
         (cl-hash-util:collect names
           (gadgets:aif2only
            (apply #'ubiquitous:value (list* 'users username names))
            anaphora:it
            (getf tspec :initial))))))))

;;FIXME: Need to add hooks or triggers for external value watching funcs.
(defun set-user-data (username &rest key/s-and-values)
  "This function does no safety checking!"
  (ubiquitous:with-transaction ()
    (with-userfig-restored
      (gadgets:map-by-2
       (lambda (keys value)
         (setf (apply #'ubiquitous:value
                      (list* 'users username (ensure-list keys)))
               value))
       key/s-and-values))))

(defun userfig-value (&rest keys)
  (ubiquitous:with-transaction ()
    (with-userfig-restored
      (check-init)
      (apply #'ubiquitous:value (list* 'users (what-user?) keys)))))

(defun (setf userfig-value) (set-to &rest keys)
  (ubiquitous:with-transaction ()
    (with-userfig-restored
      (check-init)
      (setf (apply #'ubiquitous:value 'users (what-user?) keys) set-to))))

(defun userfig-value-for (user &rest keys)
  (let ((*userfig-user* user))
    (apply #'userfig-value keys)))

(defun (setf userfig-value-for) (set-to user &rest keys)
  (let ((*userfig-user* user))
    (ubiquitous:with-transaction ()
      (with-userfig-restored
        (check-init)
        (setf (apply #'ubiquitous:value 'users (what-user?) keys) set-to)))))

(defun map-users (func)
  "Map over all of the users in userfig. Func is passed 2 parameters:
the user name and a hash table containing user settings."
  (ubiquitous:with-transaction ()
    (with-userfig-restored
      (let ((users (gethash 'users ubiquitous:*storage*)))
        (when (hash-table-p users)
          (gadgets:collecting
             (dolist (username (alexandria:hash-table-keys users))
               (gadgets:collect
                   (funcall func username (gethash username users))))))))))

(defun get-user-list ()
  (map-users (lambda (name data) (declare (ignore data)) name)))

(defun remove-user (username)
  "WARNING: removes user specified by username with all settings from userfig."
  (with-userfig-restored
    (unless (ubiquitous:value 'userfig::users username)
      (error "User does not exist"))
    (ubiquitous:remfield
     (ubiquitous:value 'userfig::users) username)
    (ubiquitous:offload)))

;;;;;;
;;; End ubiquitous stuff
;;;;;;

(defun initialized? ()
  (with-userfig-restored
    (ubiquitous:value 'users (what-user?) 'user-initialized-p)))

(defun check-init ()
  (unless (initialized?)
    (error "User is not initialized")))

;;;FIXME: Could just check for the username key, right?
(defun new-user-p (username)
  "Make sure that username does not refer to an initialized user."
  (declare (type (or string symbol) username))
  (with-userfig-restored
    (not (ubiquitous:value 'users username 'user-initialized-p))))

;;;FIXME: Will need some defense against exceedingly long user names.
(defun initialize-user (username fieldspecs)
  (apply #'set-user-data
         username
         (gadgets:collecting
             (do-fieldspecs (names tspec fieldspecs)
               (gadgets:collect names)
               (gadgets:collect (let ((init (getf tspec :initial)))
                                  (if (functionp init)
                                      (funcall init)
                                      init))))
           (gadgets:collect 'user-initialized-p)
           (gadgets:collect t))))

(defun get-user-visible-data (username fieldspecs)
  (let ((data (get-user-data username fieldspecs)))
    (cl-hash-util:collecting-hash-table (:mode :replace :test #'equal)
      (do-fieldspecs (names tspec fieldspecs)
        (when (getf tspec :viewable)
          (cl-hash-util:collect names (gethash names data)))))))

(defun validate-field (value spec)
  (multiple-value-bind (data signal)
      (funcall (getf spec :compiled-validator) value)
    (if signal
        data
        (error data))))

(defun update-from-user (username fieldspecs data-hash)
  ;;data-hash doesn't need to have all of the fields in fieldspecs
  ;;FIXME: Maybe shouldn't update unchanged fields. Perhaps shouldn't
  ;; validate them either?
  (let* ((keys (alexandria:hash-table-keys data-hash))
         (setkeys nil)
         (data
          (gadgets:collecting
              (do-fieldspecs (names spec fieldspecs)
                (multiple-value-bind (value signal) (gethash names data-hash)
                  (when signal
                    (if (getf spec :editable)
                        (progn
                          (push names setkeys)
                          (gadgets:collect names)
                          (gadgets:collect (validate-field value spec)))
                        (error "Attempt to write to read-only field"))))))))
    (unless (eq (length keys) (length setkeys))
      (error "Attempt to write to non-existent field"))
    (apply #'set-user-data username data)))

