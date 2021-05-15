# sly-ros
![alt text](./screenshots/sample.gif)

This is the port of slime_ros, the origin work can be found [here](https://github.com/code-iai/ros_emacs_utils).

It contains mostly `slime` -> `sly`, `swank` -> `slynk` replacements.

## Prerequisite
- [rosemacs](http://wiki.ros.org/rosemacs)
  - `sudo apt install ros-DISTRO-rosemacs`

## Melpa-less install

Since this is an external contrib with both Elisp and Lisp parts,
merely loading the Elisp will have little effect. The contrib has to
be registered in SLY's `sly-contribs` variable for SLY to take care of
loading the Lisp side on demand.

For convenience, the `sly-ros-autoloads` file takes care
of this automatically. So the following setup in your `~/.emacs` or
`~/.emacs.d/init/el` init file should be enough:

```elisp
;;; regular SLY setup
(setq inferior-lisp-program "/path/to/your/preferred/lisp")
(add-to-list 'load-path "/path/to/sly")
(require 'sly-autoloads)

;;; you can hardcode the distro if you don't always source ROS setup file
(add-to-list 'load-path (format "/opt/ros/%s/share/emacs/site-lisp" (getenv "ROS_DISTRO")))

(add-to-list 'load-path "/path/to/sly-ros")
(require 'sly-ros-autoloads)
```

In case you already have SLY loaded and running, you might have to
`M-x sly-setup` and `M-x sly-enable-contrib` to enable it.

## Notes
- For kinetic version, if you're using `yasnippet` then you'd probably like to delete the [legacy one](https://github.com/code-iai/ros_emacs_utils/blob/kinetic/rosemacs/yasnippet.el) at `/opt/ros/kinetic/share/emacs/site-lisp/yasnippet.el` that might cause conflict
