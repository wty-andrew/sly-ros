(in-package :asdf)

(defsystem :slynk-ros
  :depends-on (#:slynk)
  :components ((:file "slynk-ros")))
