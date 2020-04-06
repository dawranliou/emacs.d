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
(setq-default indent-tabs-mode nil)
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
    "qq" 'save-buffers-kill-terminal
    "qz" 'delete-frame

    "b" '(:ignore t :which-key "buffer")
    "bd" 'kill-this-buffer

    "f" '(:ignore t :which-key "file")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fs" 'save-buffer

    "t" '(:ignore t :which-key "toggle")
    "tw" 'whitespace-mode))

(use-package evil
  :config
  (setq evil-move-beyond-eol t)
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

(use-package git-gutter-fringe+
  :general
  (tyrant-def
    "tg" 'global-git-gutter+-mode)
  :config
  (global-git-gutter+-mode t)
  (git-gutter+-toggle-fringe)
  (setq git-gutter-fr+-side 'right-fringe))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

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

;; programming

(defvar generic-lisp-mode-hook nil)
(add-hook 'emacs-lisp-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)
  (require 'flycheck-clj-kondo))

(add-hook 'clojure-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))
(add-hook 'clojurescript-mode-hook (lambda () (run-hooks 'generic-lisp-mode-hook)))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package lispy
  :hook (generic-lisp-mode . lispy-mode))

(use-package lispyville
  :init
  (general-add-hook 'generic-lisp-mode-hook #'lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators
                              c-w
                              additional
                              additional-movement
                              slurp/barf-cp
                              prettify)))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package cider
  :general
  (general-nmap
   :keymaps 'cider-mode-map
   :prefix ","
   "" '(:ignore t :which-key "cider")
   "g" '(:ignore t :which-key "goto")
   "gg" 'cider-find-var)
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (setq cider-repl-use-pretty-printing t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets)))))

;; theme

(use-package eink-theme)

(set-face-attribute 'default nil
                    :family "Monolisa"
                    :height 120)

(use-package hl-line
  :init
  (global-hl-line-mode))

(custom-theme-set-faces
 'eink
 '(font-lock-string-face ((t (:foreground "#00449E"))))
 '(font-lock-comment-face ((t (:foreground "#a0a0a0"))))
 '(font-lock-doc-face ((t (:foreground "#a0a0a0"))))
 '(mode-line ((t (:inverse-video t))))
 '(mode-line-inactive ((t (:underline t))))
 '(hl-line ((t (:background "#FFE0E0")))))

(setq-default mode-line-format
              (list
               '(:eval (substring vc-mode 5))
               '(:eval (propertize " %b"
                                   'help-echo (buffer-file-name)))
               " (%02l,%02c)"
               " [%p/%I]"
               " %m"))

(provide 'init)
;;; Init.el ends here
