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
  "Define the `sly-ros' contrib."
  (:slynk-dependencies slynk-ros))

(defun sly-ros ()
  "Interactive command made available in lisp-editing files."
  (interactive)
  (let ((results (sly-eval '(slynk-ros:hello-world))))
    (sly-message (cl-first results))))

;;;###autoload
(with-eval-after-load 'sly
  (add-to-list 'sly-contribs 'sly-ros 'append))

(provide 'sly-ros)
