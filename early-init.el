;;; early-init.el --- Early init -*- lexical-binding: t; no-byte-compile: t; -*-

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
;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; - Startup performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(defvar doom--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      ;; package-quickstart t
      file-name-handler-alist nil
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 100 1024 1024) ; 100mb
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
                (width . 85)
                (vertical-scroll-bars . nil)
                (horizontal-bar . nil)
                (font . "Iosevka Fixed-12")))

(set-face-attribute 'default nil :family "Iosevka Fixed" :height 120)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed" :height 110 :weight 'semi-light :width 'expanded)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 110 :weight 'regular)

;;; package.el
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(provide 'early-init)

;;; early-init.el ends here
