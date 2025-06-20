;;; spacious-padding-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from spacious-padding.el

(autoload 'spacious-padding-set-faces "spacious-padding" "\
Make window dividers invisible and add padding.
Ignore any arguments.  This is useful to add the function to abnormal
hooks that pass one or more arguments to it, such as
`after-make-frame-functions'.

(fn &rest _)")
(autoload 'spacious-padding-set-parameters-of-frame "spacious-padding" "\
Set the layout parameters of FRAME and update the faces.

(fn FRAME)")
(autoload 'spacious-padding-set-parameters-of-selected-frame "spacious-padding" "\
Use `spacious-padding-set-parameters-of-frame' for the `selected-frame'.")
(defvar spacious-padding-mode nil "\
Non-nil if Spacious-Padding mode is enabled.
See the `spacious-padding-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `spacious-padding-mode'.")
(custom-autoload 'spacious-padding-mode "spacious-padding" nil)
(autoload 'spacious-padding-mode "spacious-padding" "\
Increase the padding/spacing of frames and windows.

This is a global minor mode.  If called interactively, toggle the
`Spacious-Padding mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='spacious-padding-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "spacious-padding" '("spacious-padding-"))

;;; End of scraped data

(provide 'spacious-padding-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; spacious-padding-autoloads.el ends here
