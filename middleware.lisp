(in-package :userfig)

(defparameter *userfig-url-path* "/userfig")
(defparameter *userfig-path-separator* "__")

(defun path-internal->external (pathspec)
  (format nil
          (concatenate 'string "~{~a~^" *userfig-path-separator* "~}")
          pathspec))

(defun path-external->internal (pathspec)
  (gadgets:split-sequence-on-subseq *userfig-path-separator* pathspec))

(defun userfig-component (fieldspecs &key (url-path *userfig-url-path*))
  (lambda (app)
    (let ((vspecs (validate-fieldspecs fieldspecs)))
      (lambda (env)
        (if (gadgets:sequence-starts-with (getf env :path-info) url-path)
            (let* ((subpath (subseq (getf env :path-info) (length url-path)))
                   (session (getf env :lack.session))
                   (user (gethash :username session)))
              (cond
                ((gadgets:sequence-starts-with subpath "/get-user-info")
                 `(200 (:content-type "text/json")
                       (,(cl-json:encode-json
                          (get-user-visible-data user vspecs)))))
                ((gadgets:sequence-starts-with subpath "/set-user-info")
                 (let ((params
                        (http-body:parse (getf env :content-type)
                                         (getf env :content-length)
                                         (getf env :raw-body))))
                   (print params)))
                ((gadgets:sequence-starts-with subpath "/settings")
                 (settings-page env))))
            (funcall app env))))))

(defun settings-page (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("here!!")))
