;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;;

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (defun dawran/set-default-gc ()
            (setq gc-cons-threshold (* 100 1024 1024) ; 100mb
                  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold (* 100 1024 1024)))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Alternatively, restore it even later:
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist doom--file-name-handler-alist)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (defun dawran/print-start-up-stats ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Enable narrowing by default.
(put 'narrow-to-region 'disabled nil)

;; Switch to help buffer when it's opened.
(setq help-window-select t)

(setq tramp-default-method "ssh")

(add-hook
 'after-init-hook
 (defun dawran/load-private-lisp ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; Keep backup files and auto-save files in the backups directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq straight-use-package-by-default t
      straight-build-dir (format "build-%s" emacs-version)
      ;; Lazy modification detection speeds up the startup time. I don't often
      ;; modify packages anyway. When I do, I can build the package manually, I
      ;; think.
      straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-verbose t)

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(setq-default delete-by-moving-to-trash t)

;; Both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)

;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; Mouse Scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control))))
(setq mouse-wheel-progressive-speed nil)

;; Make keybindings feel natural on mac
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-=") 'text-scale-adjust)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-<backspace>") 'kill-whole-line)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  :custom
  (evil-undo-system 'undo-fu)
  (evil-symbol-word-search t)
  (evil-want-fine-undo t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
  (define-key evil-normal-state-map "\C-y" 'yank)
  (define-key evil-insert-state-map "\C-y" 'yank)
  (define-key evil-visual-state-map "\C-y" 'yank)
  (define-key evil-normal-state-map "\C-k" 'kill-line)
  (define-key evil-insert-state-map "\C-k" 'kill-line)
  (define-key evil-visual-state-map "\C-k" 'kill-line)

  ;; Get around faster
  (define-key evil-motion-state-map "gs" 'evil-avy-goto-symbol-1)
  (define-key evil-motion-state-map "gS" 'evil-avy-goto-char-timer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; Let emacs bindings for M-. and M-, take over
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)

  (global-set-key (kbd "s-w") 'evil-window-delete)

  ;; https://blog.meain.io/2020/emacs-highlight-yanked/
  (defun dawran/evil-yank-advice (orig-fn beg end &rest args)
    "Pulse momentary on yank text"
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around #'dawran/evil-yank-advice))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer dawran/leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer dawran/local-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :major-modes t
    :prefix ","
    :non-normal-prefix "C-,")

  (dawran/leader-def
    "f"  '(:ignore t :which-key "file")
    "fd" `(,(defun dawran/find-config ()
              (interactive)
              (find-file (expand-file-name "~/.emacs.d/init.el")))
           :which-key "edit config")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(dawran/load-theme :which-key "choose theme")
    "tw" 'whitespace-mode
    "tm" 'toggle-frame-maximized
    "tM" 'toggle-frame-fullscreen))

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-M-j") #'switch-to-buffer)
(global-set-key (kbd "M-:") 'pp-eval-expression)
(global-set-key (kbd "M-/") #'hippie-expand)

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (diminish 'which-key-mode)
  (setq which-key-idle-delay 1))

(setq inhibit-startup-message t)

(setq frame-inhibit-implied-resize t)

(setq default-frame-alist
      (append (list
               '(font . "Monolisa-14")
               '(min-height . 1) '(height     . 45)
               '(min-width  . 1) '(width      . 81)
               )))

;; No beeping nor visible bell
(setq ring-bell-function #'ignore
      visible-bell nil)

(blink-cursor-mode 0)

(setq-default fill-column 80)
(setq-default line-spacing 1)

(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'lisp-interaction-mode)
    (define-key map (kbd "C-c C-c") 'lisp-interaction-mode)
    map)
  "Keymap for `scratch-mode'.")

(define-derived-mode scratch-mode
  fundamental-mode
  "Scratch"
  "Major mode for the *scratch* buffer.\\{scratch-mode-map}"
  (setq-local indent-line-function 'indent-relative))

(setq initial-major-mode 'scratch-mode)
(setq initial-scratch-message nil)

(defun jump-to-scratch-buffer ()
  "Jump to the existing *scratch* buffer or create a new one."
  (interactive)
  (let ((scratch-buffer (get-buffer-create "*scratch*")))
    (unless (derived-mode-p 'scratch-mode)
      (with-current-buffer scratch-buffer
        (scratch-mode)))
    (switch-to-buffer scratch-buffer)))

(global-set-key (kbd "s-t") #'jump-to-scratch-buffer)

(column-number-mode)

;; Enable line numbers for prog modes only
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package idle-highlight-mode
  :custom-face
  (idle-highlight ((t (:inherit lazy-highlight))))
  :hook
  (prog-mode . idle-highlight-mode))

(use-package pulse
  :straight nil
  :defer 2
  :custom-face
  (pulse-highlight-start-face ((t (:inherit highlight))))
  :config
  ;;https://karthinks.com/software/batteries-included-with-emacs/
  (defun dawran/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command
                     scroll-down-command
                     evil-scroll-up
                     evil-scroll-down
                     recenter-top-bottom
                     reposition-window
                     other-window))
    (advice-add command :after #'dawran/pulse-line)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package sketch-themes
  :straight (:host github :repo "dawranliou/sketch-themes"))

(defvar dawran/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun dawran/load-theme-action (theme)
  "Disable current themes and load theme THEME."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern theme) t)
        (run-hooks 'dawran/after-load-theme-hook))
    (error "Problem loading theme %s" theme)))

(defun dawran/load-theme ()
  "Disable current themes and load theme from the completion list."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes)))))
    (dawran/load-theme-action theme)))

(dawran/load-theme-action "sketch-black")

(defun font-lock-mode-disable ()
  (interactive)
  (font-lock-mode -1))

;;(add-hook 'prog-mode-hook #'font-lock-mode-disable)

;; Use the same font as default
(set-face-attribute 'fixed-pitch nil :font "Monolisa" :height 140)

;; Scale up the variable-pitch mode
(set-face-attribute 'variable-pitch nil :height 160)

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package paren-face
  :hook
  (lispy-mode . paren-face-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner-mode
  :straight nil
  :bind (:map evil-window-map
              ("u" . winner-undo)
              ("U" . winner-redo))
  :general
  (dawran/leader-def
    "w" 'evil-window-map)
  :config
  (winner-mode))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (diminish 'auto-fill-function))

;; Pretty much the default mode line but here's the twist: no git branch info.
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                evil-mode-line-tag
                ;; (vc-mode vc-mode)
                "  "
                mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; Inspired by diminish.el
;; https://github.com/myrjola/diminish.el/blob/master/diminish.el
(defun diminish (mode)
  "Diminish minor mode MODE."
  (let ((minor (assq mode minor-mode-alist)))
    (when minor
      (setcdr minor (list "")))))

(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(with-eval-after-load "evil-collection-unimpaired"
  (diminish 'evil-collection-unimpaired-mode))

(defun dawran/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :commands visual-fill-column-mode)

(use-package unicode-fonts
  :defer t
  :config
  (unicode-fonts-setup))

(use-package ns-auto-titlebar
  :hook (after-init . ns-auto-titlebar-mode))

(setq ns-use-proxy-icon nil
      frame-title-format nil)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package hippie-exp
  :straight nil
  :commands hippie-expand
  :custom
  (hippie-expand-try-functions-list
   '(try-complete-file-name-partially
     try-complete-file-name
     ;; try-expand-all-abbrevs
     ;; Avoid unbalanced parentheses.
     ;; try-expand-list
     ;; try-expand-line
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     ;; try-complete-lisp-symbol-partially
     ;; try-complete-lisp-symbol
     )))

(use-package orderless
  :after selectrum
  :custom
  (completion-styles '(orderless))
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

(setq enable-recursive-minibuffers t)

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-j" . selectrum-next-candidate)
         ("C-k" . selectrum-previous-candidate))
  :custom
  (selectrum-count-style 'current/matches)
  (selectrum-fix-minibuffer-height t)
  :init
  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  (setq marginalia-annotators '(marginalia-annotators-light
                                marginalia-annotators-heavy)))

;; Package `ctrlf' provides a replacement for `isearch' that is more
;; similar to the tried-and-true text search interfaces in web
;; browsers and other programs (think of what happens when you type
;; ctrl+F).
(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :bind
  ("s-f" . ctrlf-forward-fuzzy)

  :init
  (ctrlf-mode +1)

  :config
  (defun ctrlf-toggle-fuzzy ()
    "Toggle CTRLF style to `fuzzy' or back to `literal'."
    (interactive)
    (setq ctrlf--style
          (if (eq ctrlf--style 'fuzzy) 'literal 'fuzzy)))

  (add-to-list 'ctrlf-minibuffer-bindings
               '("s-f" . ctrlf-toggle-fuzzy))

  :general
  (:states '(motion)
           "*" 'ctrlf-forward-symbol-at-point
           "#" 'ctrlf-forward-symbol-at-point))

(use-package embark
  :bind
  (("C-M-," . embark-act)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-M-," . embark-act))
  :init
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  :config
  ;; Refresh candidate list after action
  (defun refresh-selectrum ()
    (setq selectrum--previous-input-string nil))
  (add-hook 'embark-pre-action-hook #'refresh-selectrum))

(use-package helpful
  :defer 2
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-key]      . #'helpful-key)
         ([remap describe-symbol]   . #'helpful-symbol)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h C"   . #'helpful-command)
         ("C-h F"   . #'describe-face)))

(use-package persistent-scratch
  :custom
  (persistent-scratch-autosave-interval 60)
  :config
  (persistent-scratch-setup-default))

(use-package recentf
  :defer 1
  :custom
  ;; Increase recent entries list from default (20)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode +1))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; I like the exaggerated tab width of 8 characters.
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode))
  :custom
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (ws-butler-keep-whitespace-before-point nil)
  :config
  (diminish 'ws-butler-mode))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (diminish 'lispy-mode)
  ;; Disable all non-evil lispy mappings.
  ;; NOTE: setting `lispy-key-theme' to nil during :init or :custom doesn't work
  ;; to achieve this.
  (lispy-set-key-theme nil)

  (general-define-key
   :keymaps 'lispy-mode-map
   :states '(normal visual)
   "C-a" 'lispy-move-beginning-of-line
   "C-e" 'lispy-move-end-of-line)

  (general-define-key
   :keymaps 'lispy-mode-map
   :states '(insert)
   "RET" 'lispy-newline-and-indent-plain
   "<backspace>" 'lispy-delete-backward
   "(" 'lispy-parens
   ")" 'lispy-right-nostring
   "\"" 'lispy-doublequote
   "[" 'lispy-brackets
   "]" 'lispy-close-square
   "{" 'lispy-braces
   "}" 'lispy-close-curly))

(use-package lispyville
  :after lispy
  :hook (lispy-mode . lispyville-mode)
  :custom
  (lispyville-key-theme '(operators
                          c-w
                          text-objects
                          additional-insert
                          additional-movement
                          additional-wrap
                          commentary
                          slurp/barf-cp))
  :config
  (diminish 'lispyville-mode)
  (lispyville-set-key-theme)
  (lispyville--define-key '(motion normal)
    "Q" 'lispy-ace-paren)
  (lispyville--define-key 'normal
    (kbd "M-j") #'lispyville-drag-forward
    (kbd "M-k") #'lispyville-drag-backward)
  (advice-add 'lispyville-yank :around 'dawran/evil-yank-advice))

(use-package iedit
  :bind
  (:map evil-visual-state-map
        ("R" . iedit-mode))
  (:map iedit-mode-occurrence-keymap
        ("RET" . iedit-toggle-selection))
  :config
  (evil-define-minor-mode-key 'normal 'iedit-mode-keymap
    (kbd "C-n") 'iedit-next-occurrence)
  (evil-define-minor-mode-key 'normal 'iedit-mode-keymap
    (kbd "C-p") 'iedit-prev-occurrence)
  (evil-define-minor-mode-key 'normal 'iedit-mode-keymap
    [remap keyboard-escape-quit] 'iedit--quit)
  (evil-define-minor-mode-key 'normal 'iedit-mode-keymap
    [remap keyboard-quit] 'iedit--quit)
  (evil-define-minor-mode-key 'normal 'iedit-mode-keymap
    [remap evil-force-normal-state] 'iedit--quit))

(use-package undo-fu
  :defer t)

(use-package elec-pair
  :straight nil
  :defer 2
  :config
  (electric-pair-mode 1)
  (add-hook 'minibuffer-setup-hook
            (defun disable-electric-pair-mode ()
              (electric-pair-mode 0))))

(use-package expand-region
  :bind
  ("s-'" .  er/expand-region)
  ("s-\"" .  er/contract-region)
  :hook
  (prog-mode . my/greedy-expansion-list)
  :config
  (defun my/greedy-expansion-list ()
    "Skip marking words or inside quotes and pairs"
    (setq-local er/try-expand-list
                (cl-set-difference er/try-expand-list
                                   '(er/mark-word
                                     er/mark-inside-quotes
                                     er/mark-inside-pairs)))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file "~/.emacs.d/savehist")
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring
     mark-ring global-mark-ring
     search-ring regexp-search-ring))
  (history-length 20000))

(use-package saveplace
  :defer 2
  :config
  (save-place-mode t))

(defun dawran/org-mode-setup ()
  (setq-local evil-auto-indent nil)
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))

(use-package org
  :hook ((org-mode . dawran/org-mode-setup)
         (org-mode . visual-line-mode)
         (org-mode . dawran/visual-fill)
         (org-mode . auto-fill-mode))
  :bind
  (:map org-mode-map
        ("C-," . nil))
  :custom
  ;; (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-cycle-separator-lines 2)
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  (org-indirect-buffer-display 'current-window)
  (org-hide-block-startup nil)
  (org-src-preserve-indentation nil)
  (org-adapt-indentation nil)
  ;; (org-startup-folded 'content)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-image-actual-width 640)
  (org-attach-auto-tag "attachment"))

(use-package org-tempo
  :straight nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package evil-org
  :after evil
  :hook (org-mode . evil-org-mode)
  :config
  (diminish 'evil-org-mode))

(use-package org-journal
  :general
  (dawran/leader-def
    "n" '(:ignore t :which-key "notes")
    "nj" '(org-journal-open-current-journal-file :which-key "journal")
    "nJ" '(org-journal-new-entry :which-key "new journal entry"))
  :custom
  (org-journal-date-format "%A, %d/%m/%Y")
  (org-journal-date-prefix "* ")
  (org-journal-file-format "%F.org")
  (org-journal-dir "~/org/journal/")
  (org-journal-file-type 'weekly)
  (org-journal-find-file #'find-file))

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam/")
  :general
  (dawran/leader-def
    "nf" 'org-roam-find-file
    :keymaps 'org-roam-mode-map
    "nl" 'org-roam
    "ng" 'org-roam-graph-show
    :keymaps 'org-mode-map
    "ni" 'org-roam-insert
    "nI" 'org-roam-insert-immediate))

(use-package org-tree-slide
  :commands (org-tree-slide-mode)
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-activate-message "Presentation started.")
  (org-tree-slide-deactivate-message "Presentation ended.")
  (org-tree-slide-breadcrumbs " > ")
  (org-tree-slide-header t))

(defvar org-paste-clipboard-image-dir "img")

(defun dawran/org-paste-clipboard-image ()
  "Paste clipboard image to org file."
  (interactive)
  (if (not (executable-find "pngpaste"))
      (message "Requires pngpaste in PATH")
    (unless (file-exists-p org-paste-clipboard-image-dir)
      (make-directory org-paste-clipboard-image-dir t))
    (let ((image-file (format "%s/%s.png"
                              org-paste-clipboard-image-dir
                              (make-temp-name "org-image-paste-"))))
      (call-process-shell-command (format "pngpaste %s" image-file))
      (insert (format  "#+CAPTION: %s\n" (read-string "Caption: ")))
      (insert (format "[[file:%s]]" image-file))
      (org-display-inline-images))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "s-y") #'dawran/org-paste-clipboard-image))

(use-package dired
  :straight nil
  :hook (;; (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :bind ("C-x C-j" . dired-jump)
  :general
  (dawran/leader-def
    "d" '(dired-jump :which-key "dired"))
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches "-AFhlv --group-directories-first")
  :init
  (setq insert-directory-program "gls")
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode))

(use-package dired-x
  :after dired
  :straight nil
  :init (setq-default dired-omit-files-p t)
  :config
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(use-package dired-ranger
  :after dired
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(use-package dired-toggle
  :general
  (dawran/leader-def
    "td" 'dired-toggle)
  :straight nil
  :load-path "lisp/")

(use-package find-dired
  :straight nil
  :defer t
  :custom
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(defun dawran/eshell-history ()
  "Browse eshell history."
  (interactive)
  (let ((candidates (cl-remove-duplicates
                     (ring-elements eshell-history-ring)
                     :test #'equal :from-end t))
        (input (let ((input-start (save-excursion (eshell-bol)))
                     (input-end (save-excursion (end-of-line) (point))))
                 (buffer-substring-no-properties input-start input-end))))
    (let ((selected (completing-read "Eshell history:"
                                     candidates nil nil input)))
      (end-of-line)
      (eshell-kill-input)
      (insert (string-trim selected)))))

(defun dawran/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use Ivy to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'dawran/eshell-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-a") 'eshell-bol)

  (setq eshell-history-size          10000
        eshell-buffer-maximum-lines  10000
        eshell-hist-ignoredups           t
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . dawran/configure-eshell)
  :general
  (dawran/leader-def
    "e" 'eshell))

(use-package exec-path-from-shell
  :defer 1
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t))

(use-package eshell-toggle
  :custom
  (eshell-toggle-use-git-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("C-M-'" . eshell-toggle)
  :general
  (dawran/leader-def
    "te" 'eshell-toggle))

(use-package project
  :bind
  (("s-p" . project-find-file)
   :map project-prefix-map
   ("m" . magit-project-status))
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit")))

(use-package magit
  :bind (("s-g" . magit-status)
         ("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :commands (magit-project-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (dawran/leader-def
    "g"   '(:ignore t :which-key "git")
    "gg"  'magit-status
    "gb"  'magit-blame-addition
    "gd"  'magit-diff-unstaged
    "gf"  'magit-file-dispatch
    "gl"  'magit-log-buffer-file))

(use-package rg
  :bind ("s-F" . rg-project)
  :config
  (rg-enable-default-bindings))

(use-package flycheck
  :ensure t
  :hook (prog-init . flycheck-mode))

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . (lambda () (setq-local idle-highlight-mode nil))))
  :custom-face
  (lsp-face-highlight-textual ((t (:inherit lazy-highlight))))
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "s-l")
  (lsp-enable-indentation nil)
  (lsp-clojure-custom-server-command '("bash" "-c" "/usr/local/bin/clojure-lsp"))
  (lsp-completion-provider :none)
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-modeline-code-actions-enable nil)
  :config
  (setq-default read-process-output-max (* 1024 1024)))

(use-package flycheck-clj-kondo
  :disabled t
  :defer t)

(use-package clojure-mode
  :defer t
  :custom
  (cljr-magic-requires nil)
  :config
  ;; (require 'flycheck-clj-kondo)
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t))

(use-package clj-refactor
  :defer t
  :config
  (diminish 'clj-refactor-mode))

(use-package cider
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  :hook
  (cider-repl-mode . evil-insert-state)
  :general
  (dawran/local-leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map)
    "," 'cider
    "e" '(:ignore t :which-key "eval")
    "eb" 'cider-eval-buffer
    "ef" 'cider-eval-defun-at-point
    "eF" 'cider-pprint-eval-defun-to-comment
    "ee" 'cider-eval-last-sexp
    "eE" 'cider-pprint-eval-last-sexp-to-comment
    "t" '(:ignore t :which-key "test")
    "tt" 'cider-test-run-test
    "tn" 'cider-test-run-ns-tests))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package go-mode
  :mode "\\.go\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook ((markdown-mode . dawran/visual-fill)
         (markdown-mode . auto-fill-mode))
  :config
  (setq markdown-command "marked"))

(use-package markdown-toc
  :commands (markdown-toc-generate-toc))

(use-package emmet-mode
  :hook
  (html-mode . emmet-mode)
  (css-mode . emmet-mode))

(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package fennel-mode
  :straight (:host gitlab :repo "technomancy/fennel-mode")
  :mode "\\.fnl\\'"
  :hook (fennel-mode . lispy-mode))

(use-package flyspell
  :straight nil
  :bind
  (:map flyspell-mode-map
        ("C-," . nil)
        ("C-;" . nil))
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))

(setq inferior-lisp-program "sbcl")

(use-package slime
  :commands slime
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el")))

(use-package compile
  :straight nil
  :defer t
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :config
  (require 'ansi-color)

  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package autorevert
  :mode ("\\.log\\'" . auto-revert-tail-mode))

(use-package extras
  :straight nil
  :load-path "lisp/"
  :bind
  (("M-y" . yank-pop+)
   ("C-x C-r" . recentf-open-files+)))

(use-package time
  :straight nil
  :custom
  (display-time-world-list '(("Asia/Taipei" "Taipei")
                             ("America/Toronto" "Toronto")
                             ("America/Los_Angeles" "San Francisco")
                             ("Europe/Berlin" "Düsseldorf")
                             ("Europe/London" "GMT")))
  :general
  (dawran/leader-def
    "tc" #'display-time-world))

(use-package elfeed
  :custom
  (elfeed-feeds '(("https://dawranliou.com/atom.xml")
                  "https://ruzkuku.com/all.atom"
                  "https://ambrevar.xyz/atom.xml"
                  "https://erick.navarro.io/index.xml"
                  "https://endlessparentheses.com/atom.xml"
                  "https://www.murilopereira.com/index.xml"
                  "https://drewdevault.com/blog/index.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://technomancy.us/atom.xml"
                  "https://worace.works/atom.xml"
                  "https://clojure.org/feed.xml"
                  ("http://irreal.org/blog/?feed=rss2" emacs)
                  ("https://emacsredux.com/atom.xml" emacs)))
  :general
  (dawran/leader-def
    "R" '(elfeed :which-key "RSS")))

(use-package shr
  :defer t
  :straight nil
  :custom
  (shr-use-colors nil)
  ;;(shr-use-fonts t)
  (shr-max-image-proportion 0.5)
  (shr-image-animate nil)
  (shr-width 72)
  (shr-discard-aria-hidden t)
  (shr-cookie-policy nil))

(use-package elpher
  :commands elpher)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)

(provide 'init)

;;; init.el ends here
