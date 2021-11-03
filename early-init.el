;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; - Path fix for Mac

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/MacGPG2/bin")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/go/bin")
  (setenv "PATH" (concat "/usr/local/bin:/usr/local/MacGPG2/bin:/usr/local/go/bin:"
                         (getenv "PATH"))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      frame-inhibit-implied-resize t)

(setq-default default-frame-alist
              '((menu-bar-lines         . 0)
                (tool-bar-lines         . 0)
                (horizontal-scroll-barr . nil)
                (vertical-scroll-bars   . nil)
                (width                  . 90)
                (height                 . 40)
                (font                   . "Iosevka-15")))

(provide 'early-init)

;;; early-init.el ends here
