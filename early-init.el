;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; - Path fix for Mac

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/MacGPG2/bin")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/go/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (add-to-list 'exec-path "/Users/daw-ranliou/bin")
  (setenv "PATH" (concat "/Users/daw-ranliou/bin:/usr/local/bin:/usr/local/MacGPG2/bin:/usr/local/go/bin:/Library/TeX/texbin:"
                         (getenv "PATH"))))

;;; - Startup performance

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      ;; package-quickstart t
      file-name-handler-alist nil
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors nil)

(add-hook 'focus-out-hook #'garbage-collect)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 256 1024 1024) ; 256mb
         gc-cons-percentage 0.3
         file-name-handler-alist doom--file-name-handler-alist)
   (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; Supress warnings
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;;; - UI

(setq-default default-frame-alist
              '((tool-bar-lines . 0)
                ;; (width          . 100)
                ;; (width . 130)
                (vertical-scroll-bars . nil)
                (font           . "Iosevka-14")))

(set-face-attribute 'default nil :family "Iosevka" :height 140)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 140 :weight 'semibold)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 130 :weight 'regular)

;;; - UTF-8 everything

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'early-init)

;;; early-init.el ends here
