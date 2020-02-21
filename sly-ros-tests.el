(require 'sly)
(require 'sly-ros)
(require 'sly-tests "lib/sly-tests")

(define-sly-ert-test sly-ros-test ()
  (with-temp-buffer
    (sly-ros)))
