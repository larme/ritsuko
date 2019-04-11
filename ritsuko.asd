;;;; ritsuko.asd

(asdf:defsystem #:ritsuko
  :description "Describe ritsuko here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:vecto #:str #:local-time #:cl-arrows #:group-by)
  :components ((:file "package")
	       (:file "journal")
	       (:file "drawing")
               (:file "ritsuko")))
