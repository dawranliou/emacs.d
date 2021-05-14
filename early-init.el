;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq package-enable-at-startup nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(provide 'early-init)

;;; early-init.el ends here
