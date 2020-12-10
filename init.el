;; -*- lexical-binding: t; -*-
;; NOTE: init.el is now generated from Emacs.org.  Please edit that file in
;;       Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar dawran/default-font-size 140)
(defvar dawran/default-variable-font-size 160)

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216)))) ; 16mb

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Keep backup files and auto-save files in the backups directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "backups/" user-emacs-directory) t)))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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
  (define-key evil-motion-state-map "gs" 'evil-avy-goto-char-timer)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (global-set-key (kbd "s-w") 'evil-window-delete))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Allows you to use the selection for * and #
(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :config
  (general-create-definer dawran/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dawran/localleader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :major-modes t
    :prefix ","
    :non-normal-prefix "C-,")

  (dawran/leader-keys
    "fd" '((lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/README.org"))) :which-key "edit config")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tw" 'whitespace-mode
    "tm" 'toggle-frame-maximized
    "tM" 'toggle-frame-fullscreen))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; No beeping nor visible bell
(setq ring-bell-function #'ignore
      visible-bell nil)

(blink-cursor-mode 0)

(setq-default fill-column 80)
(setq-default line-spacing 0.1)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                markdown-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sketch-white t)

(set-face-attribute 'default nil :font "Monolisa" :height dawran/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Monolisa" :height dawran/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height dawran/default-variable-font-size :weight 'regular)

(use-package doom-themes)

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-lsp t)
  (doom-modeline-icon nil))

(use-package ivy
  :diminish
  :custom (ivy-initial-inputs-alist nil)
  :init
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((counsel-rg     . ivy--regex-plus)
          (swiper         . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (t              . ivy--regex-ignore-order)))
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-SPC" . ivy-call-and-recenter)
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("s-b" . counsel-switch-buffer)
         ("s-y" . counsel-yank-pop)
         ("s-P" . counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package swiper
  :bind ("s-f" . swiper-isearch))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :defer 1
  :after counsel)

(dawran/leader-keys
  "C-SPC" 'counsel-M-x
  "b"   '(:ignore t :which-key "buffers")
  "bb"  '(counsel-ibuffer :which-key "switch buffer")
  "bd"  '(bury-buffer :which-key "bury buffer")
  "bk"  '(kill-this-buffer :which-key "kill buffer")
  "'"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "fr"  '(counsel-recentf :which-key "recent files")
  "fj"  '(counsel-file-jump :which-key "jump to file"))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h F" . counsel-describe-face)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" default-text-scale-increase "+")
  ("k" default-text-scale-decrease "-")
  ("r" default-text-scale-reset "reset")
  ("f" nil "finished" :exit t))

(dawran/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package paren-face
  :hook
  (lispy-mode . paren-face-mode))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(defun dawran/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun dawran/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun dawran/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
    (dawran/leave-focus-mode)
    (dawran/enter-focus-mode)))

(dawran/leader-keys
  "tf" '(dawran/toggle-focus-mode :which-key "focus mode"))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
          ("u" . winner-undo)
          ("U" . winner-redo))
  :config
  (winner-mode))

(dawran/leader-keys "w" 'evil-window-map)

(use-package hl-fill-column
  :hook (prog-mode . hl-fill-column-mode)
  :config
  (set-face-attribute 'hl-fill-column-face nil
   :background (face-attribute 'shadow :background)
   :inverse-video nil))

(defun dawran/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package ns-auto-titlebar
  :hook (after-init . ns-auto-titlebar-mode))

(setq ns-use-proxy-icon nil)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode))
  :custom
  ;; ws-butler normally preserves whitespace in the buffer (but strips it from
  ;; the written file). While sometimes convenient, this behavior is not
  ;; intuitive. To the average user it looks like whitespace cleanup is failing,
  ;; which causes folks to redundantly install their own.
  (ws-butler-keep-whitespace-before-point nil))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :custom
  (lispy-close-quotes-at-end-p t)
  :config
  (lispy-set-key-theme '(lispy c-digits))
  (add-hook 'lispy-mode-hook (lambda () (modify-syntax-entry ?- "w"))))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators
                              c-w
                              (prettify insert)
                              additional
                              additional-insert
                              additional-movement
                              (atom-movement normal visual)
                              slurp/barf-cp)))

(use-package evil-multiedit
  :config
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
  (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-symbol-and-next)
  (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-symbol-and-prev)
  (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
  (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

  ;; Restore the last group of multiedit regions.
  (define-key evil-normal-state-map (kbd "C-M-d") 'evil-multiedit-restore)
  (define-key evil-visual-state-map (kbd "C-M-d") 'evil-multiedit-restore)

  ;; RET will toggle the region under the cursor
  (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; For moving between edit regions
  (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev))

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package expand-region
  :bind ("s-'" .  er/mark-outside-pairs))

(defun dawran/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (dawran/visual-fill))

(use-package org
  :hook (org-mode . dawran/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  (dawran/leader-keys
    "j" '(org-journal-new-entry :which-key "journal")))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda ()
                            (evil-org-set-key-theme '(navigation todo insert
                                                      textobjects
                                                      additional)))))
  :config
  (evil-org-set-key-theme))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(defun dawran/org-babel-tangle-config ()
  "Automatically tangle our Emacs.org config file when we save it."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "./README.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dawran/org-babel-tangle-config)))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-journal
  :custom
  (org-journal-date-prefix "* ")
  (org-journal-file-format "%F.org")
  (org-journal-dir "~/org/journal/")
  (org-journal-file-type 'weekly)
  :config
  (dawran/leader-keys
    "n" '(:ignore t :which-key "notes")
    "nj" '(org-journal-open-current-journal-file :which-key "journal")))

(use-package org-roam
  :hook (after-init . org-roam-mode)
  :commands (org-roam org-roam-insert org-roam-switch-to-buffer
             org-roam-find-file org-roam-graph-show org-roam-capture)
  :custom
  (org-roam-directory "~/org/roam/"))

(dawran/leader-keys
  "nl" 'org-roam
  "ni" 'org-roam-insert
  "nb" 'org-roam-switch-to-buffer
  "nf" 'org-roam-find-file
  "ng" 'org-roam-graph-show
  "nc" 'org-roam-capture)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil)
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-activate-message "Presentation started.")
  (org-tree-slide-deactivate-message "Presentation ended.")
  (org-tree-slide-breadcrumbs " > ")
  (org-tree-slide-header t))

(use-package dired
  :ensure nil
  :commands (dired)
  :bind ("C-x C-j" . dired-jump)
  :init
  (setq dired-auto-revert-buffer t
        dired-dwim-target t)
  :config
  (setq ls-lisp-dirs-first t
        insert-directory-program "gls"
        dired-listing-switches "-agho --group-directories-first")
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode))

(dawran/leader-keys
  "d" '(dired-jump :which-key "dired"))

(use-package dired-x
  :after dired
  :ensure nil
  :init (setq-default dired-omit-files-p t)
  :config
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(use-package dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-ranger
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(defun dawran/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use Ivy to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-a") 'eshell-bol)

  (setq eshell-history-size          10000
        eshell-buffer-maximum-lines  10000
        eshell-hist-ignoredups           t
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . dawran/configure-eshell))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t))

(dawran/leader-keys
  "e" 'eshell)

(use-package eshell-toggle
  :custom
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("C-M-'" . eshell-toggle))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :bind (("s-F" . counsel-projectile-rg)
         ("s-p" . counsel-projectile)))

(dawran/leader-keys
  "pf"  'counsel-projectile-find-file
  "pp"  'projectile-switch-project
  "pF"  'counsel-projectile-rg
  "pe"  'projectile-run-eshell
  "pv"  'projectile-run-vterm
  "pd"  'projectile-dired
  "pc"  'projectile-run-async-shell-command-in-root
  "SPC" 'counsel-projectile-find-file)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(global-set-key (kbd "s-g") 'magit-status)

(dawran/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gg"  'magit-status
  "gb"  'magit-blame-addition
  "gd"  'magit-diff-unstaged
  "gf"  'magit-file-dispatch
  "gl"  'magit-log-buffer-file)

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package lsp-mode
  :disabled
  :commands lsp
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :init
  (setq lsp-keymap-prefix "s-l")
  :config
  (lsp-enable-which-key-integration t)
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp") ;; Optional: In case `clojure-lsp` is not in your PATH
        lsp-enable-indentation nil)

  (dawran/localleader-keys
    :keymaps '(clojure-mode-map clojurescript-mode-map)
    "d" 'lsp-find-definition
    "r" 'lsp-find-references))

(use-package eglot
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurescript-mode) . ("bash" "-c" "clojure-lsp")))
  (dawran/localleader-keys
    :keymaps '(clojure-mode-map clojurescript-mode-map)
    "d" 'xref-find-definitions
    "r" 'xref-find-references))

(use-package clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t))

(use-package cider
  :config
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-use-pretty-printing t)
  (add-hook 'cider-repl-mode-hook 'evil-insert-state)
  (evil-collection-cider-setup)
  (dawran/localleader-keys
    :keymaps '(clojure-mode-map clojurescript-mode-map)
    "," 'cider
    "e" '(:ignore t :which-key "eval")
    "eb" 'cider-eval-buffer
    "ef" 'cider-eval-defun-at-point
    "ee" 'cider-eval-last-sexp
    "t" '(:ignore t :which-key "test")
    "tt" 'cider-test-run-test
    "tn" 'cider-test-run-ns-tests))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . dawran/visual-fill)
  :config
  (setq markdown-command "marked")
  (defun dawran/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))

  (defun dawran/markdown-mode-hook ()
    (dawran/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook)))

(use-package company
  :hook (;(lsp-mode . company-mode)
         (eglot-managed-mode . company-mode))
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map eglot-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay nil)
  :config
  (add-hook 'evil-local-mode-hook
            (lambda ()
              ;; Note:
              ;; Check if `company-emulation-alist' is in
              ;; `emulation-mode-map-alists', if true, call
              ;; `company-ensure-emulation-alist' to ensure
              ;; `company-emulation-alist' is the first item of
              ;; `emulation-mode-map-alists', thus has a higher
              ;; priority than keymaps of evil-mode.
              ;; We raise the priority of company-mode keymaps
              ;; unconditionally even when completion is not
              ;; activated. This should not cause problems,
              ;; because when completion is activated, the value of
              ;; `company-emulation-alist' is ((t . company-my-keymap)),
              ;; when completion is not activated, the value is ((t . nil)).
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist)))))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(setq world-clock-list '(("Asia/Taipei" "Taipei")
                         ("America/Toronto" "Toronto")
                         ("America/Los_Angeles" "San Francisco")
                         ("Europe/Berlin" "Düsseldorf")
                         ("Europe/London" "GMT")))

(dawran/leader-keys
  "tc" 'world-clock)
