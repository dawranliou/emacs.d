;;; init.el -- An emacs configuration by Daw-Ran Liou

;;; Commentary:

;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visible-bell t)

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(eval-when-compile
  (require 'package)
  (setq package-archives
	'(("org" . "http://orgmode.org/elpa/")
	  ("gnu" . "http://elpa.gnu.org/packages/")
	  ("melpa" . "https://melpa.org/packages/")))
  
  (package-initialize)
  
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package which-key
  :config
  (which-key-mode t))

(use-package general
  :after which-key
  :commands (general-override-mode
	     general-auto-unbind-keys
	     general-simulate-key)
  :config
  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))

  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  (general-define-key
   "s-=" 'text-scale-adjust
   "s-v" 'yank
   "s-c" 'evil-yank
   "s-a" 'mark-whole-buffer
   "s-x" 'kill-region
   "s-w" 'delete-window
   "s-n" 'make-frame
   "s-s" (general-simulate-key "C-x C-s")
   "s-z" 'undo-tree-undo
   "s-Z" 'undo-tree-redo)

  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (tyrant-def
    "" nil

    "TAB" 'mode-line-other-buffer

    "c" (general-simulate-key "C-c")
    "h" (general-simulate-key "C-h")
    "u" (general-simulate-key "C-u")
    "x" (general-simulate-key "C-x")

    "q" '(:ignore t :which-key "quit")
    "qq" 'kill-emacs
    "qz" 'delete-frame

    "b" '(:ignore t :which-key "buffer")
    "bd" 'kill-this-buffer

    "f" '(:ignore t :which-key "file")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fs" 'save-buffer

    "t" '(:ignore t :which-key "toggle")
    "tw" 'whitespace-mode

   ))

(use-package evil
  :config
  (evil-mode t))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  :commands (ivy-switch-buffer)
  :general
  (tyrant-def "bm" 'ivy-switch-buffer))

(use-package counsel
  :after (ivy)
  :config
  (counsel-mode t)
  :general
  (tyrant-def
    "SPC" 'counsel-M-x
    "bb" 'counsel-ibuffer
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fL" 'counsel-locate))

(use-package swiper
  :commands swiper
  :general
  (general-define-key
   "C-s" 'swiper)
  (tyrant-def
    "s" '(:ignore t :which-key "swiper")
    "ss" 'swiper))

(use-package company
  :config
  (global-company-mode t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package flycheck
  :config
  (global-flycheck-mode t)
  :general
  (tyrant-def
    "e" '(:ignore t :which-key "errors")
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors))

(use-package rg
  :commands rg
  :config
  (rg-enable-default-bindings)
  :general
  (tyrant-def
    "/" 'rg-project))

(use-package magit
  :general
  (tyrant-def
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gb" 'magit-blame))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package beacon
  :config
  (beacon-mode t))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode t))

(use-package iedit)

(use-package projectile
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :after (projectile ivy)
  :general
  (tyrant-def
    "p" '(:ignore t :which-key "projectile")
    "pd" 'counsel-projectile-dired-find-dir
    "po" 'counsel-projectile-find-other-file
    "pf" 'counsel-projectile-find-file
    "fp" 'counsel-projectile-find-file
    "pb" 'counsel-projectile-switch-to-buffer
    "bp" 'counsel-projectile-switch-to-buffer))

(use-package smartparens)

(use-package ace-window
  :config
  (setq aw-scope 'global))

(use-package idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))

(use-package paren-face
  :hook
  (prog-mode . paren-face-mode))

;; shell

(use-package eshell
  :commands 'eshell)

(use-package shell-pop
  :commands (shell-pop)
  :config
  (shell-pop--set-shell-type
   'shell-pop-shell-type
   '("eshell" "*eshell*" (lambda nil (eshell))))
  :general
  (tyrant-def "'" 'shell-pop))

;; theme

(use-package eink-theme)

(set-face-attribute 'default nil
		    :family "Monolisa"
                    :height 120)

(provide 'init)
;;; Init.el ends here
