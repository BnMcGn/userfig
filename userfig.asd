;;;; userfig.asd

(asdf:defsystem #:userfig
  :description "Describe userfig here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:anaphora
               #:gadgets
               #:ubiquitous-concurrent
               #:lack
               #:parenscript
               #:cl-react
               #:ps-gadgets
               #:cl-json
               #:webhax-validate
               #:http-body
               #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "backend")
               (:file "middleware")
               (:file "userfig")))

