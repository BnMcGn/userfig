(in-package :userfig)

(gadgets:eval-always (defparameter *userfig-url-path* "/userfig"))
(defparameter *userfig-path-separator* "__")

(defvar *session*)
(defvar *env*)

(defun session-from-env (env)
  (getf env :lack.session))

(defun path-internal->external (pathspec)
  (format nil
          (concatenate 'string "~{~a~^" *userfig-path-separator* "~}")
          pathspec))

(defun path-external->internal (pathspec fieldspecs)
  (let ((pspec
         (gadgets:split-sequence-on-subseq *userfig-path-separator* pathspec)))
    (do-fieldspecs (names fspec fieldspecs)
      (declare (ignore fspec))
      (when (every #'gadgets:eq-symb pspec names)
        names))))

(defun make-external-name-map (fieldspecs)
  (cl-hash-util:collecting-hash-table (:mode :replace :test #'equal)
    (do-fieldspecs (names fspec fieldspecs)
      (declare (ignore fspec))
      (cl-hash-util:collect (path-internal->external names) names))))

(defun prep-user-data (datahash)
  (cl-hash-util:collecting-hash-table (:mode :replace)
    (maphash
     (lambda (k v) (cl-hash-util:collect (path-internal->external k) v))
     datahash)))

(gadgets:eval-always
  (defparameter *userfig-user* nil))

;;;Will consist of a list: (username fieldspecs)
(defparameter *current-parameters* nil)

(defun userfig-component (fieldspecs &key (url-path *userfig-url-path*))
  (lambda (app)
    (let ((vspecs (validate-fieldspecs fieldspecs))
          (external-names (make-external-name-map fieldspecs)))
      (lambda (env)
        (if (gadgets:sequence-starts-with (getf env :path-info) url-path)
            (let* ((subpath (subseq (getf env :path-info) (length url-path)))
                   (session (session-from-env env))
                   (user (gethash :username session))
                   ;;FIXME: Whole userthing needs a cleanup
                   (*userfig-user* (gethash :username session))
                   (display-name (gethash :display-name session)))
              (when (and user (new-user-p user))
                (let ((*session* session)
                      (*env* env))
                  (initialize-user user vspecs)))
              (cond
                ((null user)
                 '(403 nil ("403: Not logged in")))
                ((gadgets:sequence-starts-with subpath "/get-user-info")
                 `(200 (:content-type "text/json")
                       (,(cl-json:encode-json-to-string
                          (prep-user-data
                           (get-user-visible-data user vspecs))))))
                ((gadgets:sequence-starts-with subpath "/set-user-info")
                 (handle-set-user-info user env vspecs external-names))
                ((gadgets:sequence-starts-with subpath "/settings")
                 (let ((webhax-core:*web-env* env))
                   (settings-page vspecs display-name)))))
            (let ((*current-parameters*
                   (list
                    (gethash :username (session-from-env env))
                    fieldspecs)))
              (funcall app
                       (list*
                        :userfig-initialize-user ;Pass initialize func. down
                        (lambda (user)
                          (initialize-user user vspecs))
                        env))))))))

(defun settings-url ()
  (concatenate 'string *userfig-url-path* "/settings"))

(defun what-user? ()
  (or *userfig-user*
      (car *current-parameters*)
      (error "No user name found")))

(defun handle-set-user-info (user env fieldspecs name-map)
  (multiple-value-bind (values sig)
      (webhax-validate:validate-batch
       (getf env :body-parameters)
       fieldspecs :translation-table (gadgets:invert-hash-table name-map))
    (when sig
      (update-from-user user fieldspecs values))
    `(200 (:content-type "text/json")
          (,(webhax-validate:batch-response-json values sig)))))

(defun jsonify-fieldspecs (fieldspecs)
  (cl-json:encode-json-to-string
   (gadgets:collecting
       (do-fieldspecs (names fspec fieldspecs)
         (gadgets:collect (path-internal->external names))
         (gadgets:collect (webhax-validate:prep-fieldspec-body-for-json fspec))))))

(defun userfig-js (fieldspecs)
  (ps
    (let ((fieldspecs
           (lisp-raw
            (jsonify-fieldspecs fieldspecs)))
          (data-url
           (lisp (concatenate 'string *userfig-url-path* "/get-user-info")))
          (save-url
           (lisp (concatenate 'string *userfig-url-path* "/set-user-info"))))
      (defun initialize-userfig ()
        (ps-gadgets:json-bind (data data-url)
          (render
           (psx
            (:webhax-form-connector :fieldspecs fieldspecs :data data
                                    :validation-url save-url))
           (chain document (get-element-by-id "userfig-form"))))))))

(defun settings-page (fieldspecs display-name)
  (funcall
   (webhax-route:quick-page
       (#'webhax-metaplate:react-parts
        #'webhax-metaplate:redux-parts
        :@javascript #'webhax-widgets:ps-widgets
        :@javascript
        (lambda () (userfig-js fieldspecs)))
     (webhax-core:html-out
       (:h2 (format webhax-core:*webhax-output* "Settings: ~a" display-name))
       (:div :id "userfig-form")
       ;;FIXME: This should be in body onLoad?
       ;; Might not be reliable here.
       (:script :type "text/javascript" "initializeUserfig();")))))

