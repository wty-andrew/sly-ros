(defpackage #:slynk-ros
  (:use :cl #:slynk-api)
  (:export
   #:hello-world))
(in-package #:slynk-ros)

(defslyfun hello-world ()
  "Provide hello-worldish functionality for the Emacs side of SLY"
  (list (format nil "Hello sly ros") :sly-ros))

(provide 'slynk-ros)
