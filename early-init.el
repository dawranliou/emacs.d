;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; - Path fix for Mac

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/MacGPG2/bin")
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/go/bin")
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (setenv "PATH" (concat "/usr/local/bin:/usr/local/MacGPG2/bin:/usr/local/go/bin:/Library/TeX/texbin:"
                         (getenv "PATH"))))

;;; - GC

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7
      ;; package-quickstart t
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 256 1024 1024) ; 256mb
         gc-cons-percentage 0.3)
   (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;;; - UI

(setq-default default-frame-alist
              '((menu-bar-lines         . 0)
                (tool-bar-lines         . 0)
                (horizontal-scroll-barr . nil)
                (vertical-scroll-bars   . nil)
                (width                  . 90)
                (height                 . 40)
                (font                   . "Iosevka-15")))

;;; - UTF-8 everything

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(provide 'early-init)

;;; early-init.el ends here
