;;; init.el -- An emacs configuration by Daw-Ran Liou

;;; Commentary:

;;; Code:
;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold (* 50 1000 1000))

(package-initialize)
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;; =============
;; Mac OSX

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(mac-auto-operator-composition-mode)

;; =============
;; Visuals

(set-face-attribute 'default nil :font "Monolisa 12")
(setq-default line-spacing 1)

(add-to-list 'default-frame-alist '(ns-transparent-title-bar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(add-to-list 'custom-theme-load-path "~/.emacs.d/")

(load-theme 'thirdstream t)
;; (use-package eink-theme)
;; (set-face-foreground font-lock-string-face "#00449E")
;; (set-face-foreground font-lock-comment-face "#808080")
;; (set-face-foreground font-lock-doc-face "#808080")
;; (set-face-foreground font-lock-doc-face "#808080")
;; (set-face-attribute 'fringe nil :foreground "#808080")
;; (set-face-attribute 'mode-line nil :height 1.0 :inverse-video t)
;; (set-face-attribute 'mode-line-inactive nil :height 1.0)

(setq initial-frame-alist
      '((width . 100)
        (height . 50)))

(setq-default mode-line-format
              (list
               '(:eval (propertize " %b"
                                   'help-echo (buffer-file-name)))
               " (%02l,%02c)"
               " [%p/%I]"
               " %m"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq ring-bell-function 'ignore)

(setq-default frame-title-format "%b (%f)")

(setq-default indent-tabs-mode nil)

(blink-cursor-mode 0)

(use-package rainbow-mode)

;; =============
;; Sane defaults

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files

(global-auto-revert-mode t)

(setq
 inhibit-startup-message t         ; Don't show the startup message
 inhibit-startup-screen t          ; or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil       ; Empty scratch buffer
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
)

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(global-unset-key (kbd "s-p"))

(use-package simpleclip
  :init
  (simpleclip-mode 1))

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

;; Count hyphens as word characters
(modify-syntax-entry ?- "w")

;; ========
;; Customizations

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; ========
;; Which key

(use-package which-key
  :config
  (which-key-mode t))

;; ========
;; OS integration

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; ============
;; Keymaps

(defvar my/space-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

(defvar my/clojure-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts in Clojure mode.")

;; ========
;; Evil

(use-package evil
  :config
  (evil-mode t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil))

(define-key evil-normal-state-map (kbd "SPC") my/space-map)
(define-key my/space-map (kbd "b C-d") 'my/kill-other-buffers)
(define-key my/space-map (kbd "b C-d") 'my/kill-other-buffers)
(define-key my/space-map (kbd "fed") 'my/find-user-init-file)
(define-key my/space-map (kbd "feR") 'my/load-user-init-file)
(define-key my/space-map (kbd "TAB") 'mode-line-other-buffer)
(define-key my/space-map (kbd "bd") 'kill-this-buffer)
(define-key my/space-map (kbd "Pr") 'package-autoremove)
(define-key my/space-map (kbd "Pu") 'my/package-upgrade-all)
(define-key my/space-map (kbd "qq") 'save-buffers-kill-emacs)
(define-key my/space-map (kbd "tw") 'whitespace-mode)

(use-package fill-column-indicator
  :hook
  (git-commit-mode . fci-mode)
  :init
  (setq-default fci-rule-column 80)
  :config
  (define-key my/space-map "tf" 'fci-mode))

;; ========
;; Navigation and editing

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

(use-package avy
  :config
  (global-set-key (kbd "s-;") 'avy-goto-char-timer))

(global-set-key (kbd "s-w") (kbd "C-x 0"))
(global-set-key (kbd "s-W") (kbd "C-x 1"))

(global-set-key (kbd "s-k") 'kill-this-buffer)

(use-package expand-region
  :config
  (global-set-key (kbd "s-'") 'er/expand-region)
  (global-set-key (kbd "s-\"") 'er/contract-region))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)

(global-set-key (kbd "s-/") 'comment-line)

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)

(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-n") 'split-window-vertically)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package ace-window
  :config
  (setq aw-scope 'global))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode t))

(use-package iedit)

;; ========
;; Windows

;; always split on the bottom
(setq split-height-threshold 0)
(setq split-width-threshold nil)

(winner-mode 1)

;; ========
;; Ivy, Counsel, and Swiper

(use-package ivy
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (global-set-key (kbd "s-b") 'ivy-switch-buffer)
  (global-set-key (kbd "M-s-b") 'ivy-resume))

(use-package counsel
  :config
  (counsel-mode t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-P") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-o") 'counsel-find-file)
  (global-set-key (kbd "s-y") 'counsel-yank-pop)
  (global-set-key (kbd "s-F") 'counsel-rg)
  (global-set-key (kbd "s-p") 'counsel-git)
  (define-key my/space-map "fr" 'counsel-recentf))

(use-package swiper
  :config
  (global-set-key (kbd "s-f") 'swiper-isearch)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (setq swiper-faces
        '(ivy-minibuffer-match-face-1
          ivy-minibuffer-match-face-2
          ivy-minibuffer-match-face-3
          ivy-minibuffer-match-face-4))
  (setq swiper-background-faces
        '(ivy-minibuffer-match-face-1
          ivy-minibuffer-match-face-2
          ivy-minibuffer-match-face-3
          ivy-minibuffer-match-face-4)))

(use-package smex)

;; ========
;; Git

(use-package magit
  :config
  (global-set-key (kbd "s-g s") 'magit-status)
  (global-set-key (kbd "s-g b") 'magit-blame)
  (global-set-key (kbd "s-g l") 'magit-log-all)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

;; ========
;; Spell checking

(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(global-set-key (kbd "s-\\") 'ispell-word)

;; ========
;; Parens

(require 'paren)
(show-paren-mode 1)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t))

(use-package idle-highlight-mode
  :hook
  (prog-mode . idle-highlight-mode))

(use-package paren-face
  :hook
  (prog-mode . paren-face-mode))

;; ========
;; Projectile

(use-package projectile
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t))

(use-package counsel-projectile
  :after (projectile ivy)
  :config
  (counsel-projectile-mode t)
  (global-set-key (kbd "s-F") 'counsel-projectile-rg)
  (global-set-key (kbd "s-p") 'counsel-projectile-find-file))

;; ========
;; Auto completion

(use-package company
  :config
  (global-company-mode t)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; ========

(use-package restart-emacs
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args)))
  (define-key my/space-map "qr" 'restart-emacs)
  (define-key my/space-map "qd" 'restart-emacs-debug-init))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

;; ========
;; shell

(use-package eshell
  :commands 'eshell)

(use-package shell-pop
  :commands (shell-pop)
  :config
  (shell-pop--set-shell-type
   'shell-pop-shell-type
   '("eshell" "*eshell*" (lambda nil (eshell)))))

(global-set-key (kbd "s-=") 'shell-pop)

;; ========
;; Programming

(use-package flycheck
  :config
  (global-flycheck-mode t)
  (define-key my/space-map "eb" 'flycheck-buffer)
  (define-key my/space-map "ec" 'flycheck-clear)
  (define-key my/space-map "en" 'flycheck-next-error)
  (define-key my/space-map "ep" 'flycheck-previous-error)
  (define-key my/space-map "el" 'flycheck-list-errors))

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)
  (require 'flycheck-clj-kondo))

(evil-define-key 'normal clojure-mode-map
  (kbd "s-,") my/clojure-map)

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
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
  :config
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t)
  (define-key my/clojure-map "'" 'sesman-start)
  (define-key my/clojure-map "eb" 'cider-eval-buffer)
  (define-key my/clojure-map "ee" 'cider-eval-last-sexp)
  (define-key my/clojure-map "ef" 'cider-eval-defun-at-point)
  (define-key my/clojure-map "==" 'cider-format-buffer)
  (define-key my/clojure-map "=f" 'cider-format-defun)
  (define-key my/clojure-map "=r" 'cider-format-region)
  (define-key my/clojure-map "ha" 'cider-apropos)
  (define-key my/clojure-map "hc" 'cider-cheatsheet)
  (define-key my/clojure-map "hh" 'cider-doc)
  (define-key my/clojure-map "hn" 'cider-browse-ns)
  (define-key my/clojure-map "hN" 'cider-browse-ns-all)
  (define-key my/clojure-map "hs" 'cider-browse-spec)
  (define-key my/clojure-map "hS" 'cider-browse-spec-all)
  (define-key my/clojure-map "gb" 'cider-pop-back)
  (define-key my/clojure-map "gg" 'my/clj-find-var)
  (define-key my/clojure-map "gn" 'cider-find-ns)
  (define-key my/clojure-map "gr" 'cider-find-resource)
  (define-key my/clojure-map "mq" 'sesman-quit)
  (define-key my/clojure-map "mr" 'sesman-restart)
  )

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets)))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; functions

(defun my/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\""
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun my/find-user-init-file ()
  "Edit the `user-init-file' in same window."
  (interactive)
  (find-file user-init-file))

(defun my/load-user-init-file ()
  "Load the `user-init-file' in same window."
  (interactive)
  (load-file user-init-file))

;; https://emacs.stackexchange.com/questions/16398/noninteractively-upgrade-all-packages
(defun my/package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
			   (let ((pkg (cadr (assq name where))))
			     (when pkg
			       (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(defun my/clj-find-var (sym-name &optional arg)
  "Attempts to jump-to-definition of the symbol-at-point.

If CIDER fails, or not available, falls back to dumb-jump."
  (interactive (list (cider-symbol-at-point)))
  (if (and (cider-connected-p) (cider-var-info sym-name))
      (unless (eq 'symbol (type-of (cider-find-var nil sym-name)))
        (dumb-jump-go))
    (dumb-jump-go)))

(provide 'init)
;;; Init.el ends here
