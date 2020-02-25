;;; sly-ros.el  -*- lexical-binding: t; -*-
;;
;; Version: 0.1
;; Keywords: languages, lisp, sly
;; Package-Requires: (sly rosemacs)
;;
;; Copyright (C) 2020
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Description:
;;
;; This is the SLY port of a contrib originally written for SLIME
;; to assist in working with ROS packages, with minimal changes,
;; mostly "slime"->"sly", "swank"->"slynk" replacements.
;;
;;; Code:

(require 'sly)
(require 'rosemacs)

(define-sly-contrib sly-ros
  "Extension of sly for utilizing rosemacs features."
  (:sly-dependencies sly-mrepl)
  (:slynk-dependencies slynk-ros)
  (:on-load (add-hook 'sly-connected-hook 'sly-ros-load-manifest)))

(defcustom sly-ros-completion-function 'completing-read
  "The completion function to be used for package and system
  completions. This variable can be set to `ido-completing-read'
  to enable `ido-mode' for ros packages."
  :type 'function
  :group 'rosemacs)

(defun sly-ros-load-manifest ()
  (let ((roslisp-path (ros-package-dir "roslisp")))
    (when roslisp-path
      (sly-eval-async `(slynk-ros:load-ros-manifest ,roslisp-path)
                        (lambda (result)
                          (message "Successfully loaded ros-load-manifest."))))))

(defun sly-ros-read-pkg-name (&optional prompt default-value)
  (cond ((not (sly-current-connection))
          (message "Not connected."))
        (t
         (let ((default (sly-eval `(cl:identity ros-load:*current-ros-package*))))
           (ros-completing-read-package nil default sly-ros-completion-function)))))

(defun sly-ros-replace-underscores (str)
  (replace-regexp-in-string "_" "-" str))

(defun sly-bogus-completion-alist (list)
  "Make an alist out of list. The same elements go in the CAR, and
  nil in the CDR. To support the `try-completions' interface, that
  wants an alist but ignores CDRs."
  (mapcar (lambda (x) (cons x nil)) list))

(defun sly-ros-get-systems-in-pkg (package &optional default-value prompt)
  (let* ((package-path (ros-package-path package))
         (asd-files (append (ros-files-in-package package-path "asd" "asdf")
                            (ros-files-in-package package-path "asd" ".")))
         (default2 (sly-ros-replace-underscores default-value))
         (default (cond ((member default-value asd-files) default-value)
                        ((member default2 asd-files) default2)))
         (prompt (concat (or prompt (format "ROS Package `%s', System" package))
                         (if default
                             (format " (default `%s'): " default)
                           ": "))))
    (funcall sly-ros-completion-function
             prompt (mapcar #'car (sly-bogus-completion-alist asd-files))
             nil nil nil nil default)))

(defun sly-mrepl-ros-load-system ()
  (interactive)
  (let* ((ros-pkg-name (sly-ros-read-pkg-name))
         (path (ros-package-path ros-pkg-name))
         (system-name (sly-ros-get-systems-in-pkg ros-pkg-name ros-pkg-name)))
    (sly-cd path)
    (setq default-directory path)
    (sly-eval `(cl:setf ros-load:*current-ros-package* ,ros-pkg-name))
    (message "Performing ASDF %S on system %S" 'load-op system-name)
    (sly-eval-async `(slynk-ros:ros-load-system ,system-name))))

(add-to-list 'sly-mrepl-shortcut-alist '("ros-load-system" . sly-mrepl-ros-load-system))

(defun sly-mrepl-ros-test-system ()
  (interactive)
  (let* ((ros-pkg-name (sly-ros-read-pkg-name))
         (system-name (sly-ros-get-systems-in-pkg ros-pkg-name ros-pkg-name)))
    (sly-eval `(cl:setf ros-load:*current-ros-package* ,ros-pkg-name))
    (message "Performing ASDF %S on system %S" 'load-op system-name)
    (sly-eval-async `(slynk-ros:ros-test-system ,system-name))))

(add-to-list 'sly-mrepl-shortcut-alist '("ros-test-system" . sly-mrepl-ros-test-system))

;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-ros 'append))

(provide 'sly-ros)
