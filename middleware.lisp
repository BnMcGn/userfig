(in-package :userfig)

(gadgets:eval-always (defparameter *userfig-url-path* "/userfig"))
(defparameter *userfig-path-separator* "__")

(defun path-internal->external (pathspec)
  (format nil
          (concatenate 'string "~{~a~^" *userfig-path-separator* "~}")
          pathspec))

(defun path-external->internal (pathspec)
  (gadgets:split-sequence-on-subseq *userfig-path-separator* pathspec))

(defun prep-user-data (datahash)
  (cl-hash-util:collecting-hash-table (:mode :replace)
    (maphash
     (lambda (k v) (cl-hash-util:collect (path-internal->external k) v))
     datahash)))

(defun userfig-component (fieldspecs &key (url-path *userfig-url-path*))
  (lambda (app)
    (let ((vspecs (validate-fieldspecs fieldspecs)))
      (lambda (env)
        (if (gadgets:sequence-starts-with (getf env :path-info) url-path)
            (let* ((subpath (subseq (getf env :path-info) (length url-path)))
                   (session (getf env :lack.session))
                   (user (gethash :username session))
                   (display-name (gethash :display-name session)))
              (cond
                ((null user)
                 '(403 nil ("403: Not logged in")))
                ((gadgets:sequence-starts-with subpath "/get-user-info")
                 `(200 (:content-type "text/json")
                       (,(cl-json:encode-json-to-string
                          (prep-user-data
                           (get-user-visible-data user vspecs))))))
                ((gadgets:sequence-starts-with subpath "/set-user-info")
                 (let ((params
                        (http-body:parse (getf env :content-type)
                                         (getf env :content-length)
                                         (getf env :raw-body))))
                   (print params)))
                ((gadgets:sequence-starts-with subpath "/settings")
                 `(200 (:content-type "text/html")
                       (,(settings-page vspecs display-name))))))
            (funcall app env))))))

(defun jsonify-fieldspecs (fieldspecs)
  (cl-json:encode-json-to-string
   (cl-hash-util:collecting-hash-table (:mode :replace)
     (do-fieldspecs (names fspec fieldspecs)
       (cl-hash-util:collect (path-internal->external names)
         (cl-hash-util:plist->alist
          (nth-value
           1 (gadgets:extract-keywords '(:compiled-validator) fspec))))))))

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
           (webhax-form-element
            fieldspecs data
            (lambda (data)
              (json-bind (res save-url)
                (say "Data saved"))))
           (chain document (get-element-by-id "userfig-form"))))))))

(defun settings-page (fieldspecs display-name)
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title (format s "Settings for ~a" display-name))
      (:script :type "text/javascript"
        :src "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react.js")
      (:script :type "text/javascript"
        :src "https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react-dom.js")
      (:script :type "text/javascript"
               :src "/static/javascript/redux.js")
      (:script :type "text/javascript"
               :src "/static/javascript/react-redux.js")
      (:script :type "text/javascript"
               :src "/static/javascript/jquery/1.9.1/jquery.js")
      (:script :type "text/javascript" (str (cl-react:build)))
      (:script :type "text/javascript" (str (ps-gadgets:ps-gadgets)))
      (:script :type "text/javascript" (str (webhax-widgets:ps-widgets)))
      (:script :type "text/javascript" (str (userfig-js fieldspecs))))
     (:body :onload "initializeUserfig();"
      (:h2 (format s "Settings: ~a" display-name))
      (:div :id "userfig-form" "asdf")))))

