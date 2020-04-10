;;; init.el -- An emacs configuration by Daw-Ran Liou

;;; Commentary:

;;; Code:
(package-initialize)
(setq package-archives
      '(("org" . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Core settings

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(set-language-environment "UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)

(setq initial-frame-alist
      '((width . 102)
        (height . 54)))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(mac-auto-operator-composition-mode)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(setq fringes-outside-margins t)
(setq select-enable-clipboard t)
(blink-cursor-mode 0)

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
 '(font-lock-comment-face ((t (:foreground "#808080"))))
 '(font-lock-doc-face ((t (:foreground "#808080"))))
 '(fringe ((t (:foreground "#808080"))))
 '(mode-line ((t (:inverse-video t))))
 '(mode-line-inactive ((t (:underline t))))
 '(hl-line ((t (:background "#ffdfdf")))))

(setq-default mode-line-format
              (list
               '(:eval (propertize " %b"
                                   'help-echo (buffer-file-name)))
               " (%02l,%02c)"
               " [%p/%I]"
               " %m"))

;; Packages

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package which-key
  :config
  (which-key-mode t))

(use-package general
  :config
  (general-define-key
   ;; mac
   "s-=" 'text-scale-adjust
   "s-v" 'yank
   "s-c" 'evil-yank
   "s-a" 'mark-whole-buffer
   "s-x" 'kill-region
   "s-w" 'delete-window
   "s-n" 'make-frame
   "s-s" (general-simulate-key "C-x C-s")
   "s-z" 'undo-tree-undo
   "s-Z" 'undo-tree-redo

   :keymaps 'key-translation-map
   "ESC" (kbd "C-g"))

  (general-create-definer set-leader-keys
    :states '(normal visual motion emacs)
    :prefix "SPC")
  
  (set-leader-keys
    "" nil

    "TAB" '(mode-line-other-buffer :wk "last buffer")

    "c" (general-simulate-key "C-c")
    "h" (general-simulate-key "C-h" :which-key "help")
    "u" (general-simulate-key "C-u" :which-key "universal")
    "x" (general-simulate-key "C-x")

    "a" '(:ignore t :which-key "app")
    "ad" 'dired

    "b" '(:ignore t :which-key "buffer")
    "bd" 'kill-this-buffer
    "b C-d" 'my/kill-other-buffers

    "f" '(:ignore t :which-key "file")
    "fe" '(:ignore t :which-key "emacs")
    "fed" 'my/find-user-init-file
    "feR" 'my/load-user-init-file
    "fs" 'save-buffer

    "p" '(:ignore t :which-key "project")

    "P" '(:ignore t :which-key "Packages")
    "Pr" 'package-autoremove
    "Pu" 'my/package-upgrade-all

    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal

    "s" '(:ignore t :which-key "search")

    "t" '(:ignore t :which-key "toggle")
    "tw" 'whitespace-mode))

(use-package evil
  :config
  (evil-mode t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  (set-leader-keys
    "bN" 'evil-buffer-new
    "fd" 'evil-save-and-close))

(use-package ivy
  :general
  (set-leader-keys
    "rl" 'ivy-resume)
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

(use-package counsel
  :config
  (counsel-mode t)
  :general
  (set-leader-keys
    "SPC" 'counsel-M-x
    "bb" 'counsel-ibuffer
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fL" 'counsel-locate
    "?" 'counsel-descbinds
    "iu" 'counsel-unicode-char
    "sj" 'counsel-imenu))

(use-package swiper
  :general
  (general-define-key
   "C-s" 'swiper)
  (set-leader-keys
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
  (set-leader-keys
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
  (set-leader-keys
    "/" 'rg-project))

(use-package magit
  :general
  (set-leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gb" 'magit-blame
    "gl" 'magit-log-all)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package git-gutter-fringe+
  :general
  (set-leader-keys
    "tg" 'global-git-gutter+-mode)
  :config
  (global-git-gutter+-mode t)
  (git-gutter+-toggle-fringe)
  (setq git-gutter-fr+-side 'right-fringe))

(use-package restart-emacs
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args)))
  :general
  (set-leader-keys
    "qr" 'restart-emacs
    "qd" '(restart-emacs-debug-init :which-key "quit with debug-init")))

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
  (set-leader-keys
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

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package fill-column-indicator
  :hook
  (git-commit-mode . fci-mode)
  :init
  (setq-default fci-rule-column 80)
  :general
  (set-leader-keys
    "tf" 'fci-mode))

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
  (set-leader-keys "'" 'shell-pop))

;; programming

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments)
  (setq clojure-align-forms-automatically t)
  (require 'flycheck-clj-kondo))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package lispy
  :hook ((clojure-mode . lispy-mode)
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
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps '(clojure-mode-map clojurescript-mode-map cider-mode-map)
   :prefix ","

   "'" 'sesman-start

   "e" '(:ignore t :which-key "evaluation")
   "eb" 'cider-eval-buffer
   "ee" 'cider-eval-last-sexp
   "ef" 'cider-eval-defun-at-point

   "=" '(:ignore t :which-key "format")
   "==" 'cider-format-buffer
   "=f" 'cider-format-defun
   "=r" 'cider-format-region

   "h" '(:ignore t :which-key "help")
   "ha" 'cider-apropos
   "hc" 'cider-cheatsheet
   "hh" 'cider-doc
   "hn" 'cider-browse-ns
   "hN" 'cider-browse-ns-all
   "hs" 'cider-browse-spec
   "hS" 'cider-browse-spec-all

   "g" '(:ignore t :which-key "goto")
   "gb" 'cider-pop-back
   "gg" 'my/clj-find-var
   "gn" 'cider-find-ns
   "gr" 'cider-find-resource

   "m" '(:ignore t :which-key "manage repls")
   "mq" 'sesman-quit
   "mr" 'sesman-restart
   )
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t))

(use-package aggressive-indent
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojurescript-mode-hook #'aggressive-indent-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
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
