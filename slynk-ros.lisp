(defpackage #:slynk-ros
  (:use :cl #:slynk-api)
  (:import-from #:slynk
                #:collect-notes
                #:defslyfun)
  (:import-from #:slynk-backend #:with-compilation-hooks)
  (:export #:load-ros-manifest
           #:ros-load-system
           #:ros-test-system))

(in-package #:slynk-ros)

(defmethod asdf:perform :around ((o asdf:load-op)
                                 (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    ;; If a fasl was stale, try to recompile and load (once).
    (sb-ext:invalid-fasl ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))

(defslyfun load-ros-manifest (asdf-system-directory)
  "Add appropriate paths for asdf to look for ros-load-manifest and load it."
  (unless (asdf:find-system :ros-load-manifest nil)
    (let ((load-manifest-directory
            (parse-namestring
             (concatenate 'string (namestring asdf-system-directory)
                          "/load-manifest/"))))
      (push load-manifest-directory asdf:*central-registry*)))
  (asdf:operate 'asdf:load-op :ros-load-manifest)
  (format t "~%ROS welcomes you!"))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using ASDF.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"cl-ppcre\" 'compile-op :force t)"
  (handler-case
      (with-compilation-hooks ()
        (apply #'asdf:operate operation-name system-name keyword-args)
        t)
    ((or asdf:compile-error #+asdf3 asdf/lisp-build:compile-file-error)
      () nil)))

(defslyfun ros-load-system (system-name)
  "Compile and load ROS system using ASDF."
  (collect-notes
   (lambda ()
     (operate-on-system system-name 'asdf:load-op))))

(defslyfun ros-test-system (system-name)
  "Test ROS system using ASDF."
  (collect-notes
   (lambda ()
     (operate-on-system system-name 'asdf:test-op))))

;;; Redirect all the I/O from Slynk SBCL process to standard I/O
(setf slynk:*globally-redirect-io* t)

(provide 'slynk-ros)
