;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;;

;;; Code:


;;; - Performance

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (defun dawran/set-default-gc ()
            (setq gc-cons-threshold (* 100 1024 1024) ; 100mb
                  gc-cons-percentage 0.1)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (defun dawran/print-start-up-stats ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-hook
 'after-init-hook
 (defun dawran/load-private-lisp ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))


;;; - Emacs


(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      tramp-default-method "ssh"
      backup-directory-alist `(("." . ,(expand-file-name
                                        "backups"
                                        user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name
                                               "auto-save-list/"
                                               user-emacs-directory) t))
      default-frame-alist  '((menu-bar-lines . 0)
                             (tool-bar-lines . 0)
                             (vertical-scroll-bars . nil)
                             (font . "Monolisa-14")
                             (min-width . 1)
                             (width . 81))
      ring-bell-function #'ignore
      visible-bell nil
      line-spacing 1
      ns-use-proxy-icon nil
      frame-title-format nil
      enable-recursive-minibuffers t
      recentf-max-saved-items 200
      savehist-file "~/.emacs.d/savehist"
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
                                      mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring)
      history-length 20000
      display-time-world-list '(("Asia/Taipei" "Taipei")
                                ("America/Toronto" "Toronto")
                                ("America/Los_Angeles" "San Francisco")
                                ("Europe/Berlin" "Düsseldorf")
                                ("Europe/London" "GMT"))
      custom-file (expand-file-name "custom.el" user-emacs-directory))


(load custom-file 'noerror)


(setq-default delete-by-moving-to-trash t
              fill-column 80
              tab-width 8
              indent-tabs-mode nil
              mode-line-format '("%e"
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
                                 mode-line-modes
                                 mode-line-misc-info
                                 mode-line-end-spaces))


(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(global-set-key (kbd "<escape>") #'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") #'switch-to-buffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "M-:") #'pp-eval-expression)
(global-set-key (kbd "M-Z") #'zap-to-char)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "s--") #'text-scale-decrease)
(global-set-key (kbd "s-<backspace>") #'kill-whole-line)
(global-set-key (kbd "s-=") #'text-scale-adjust)
(global-set-key (kbd "s-S") #'write-file)
(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-i") #'imenu)
(global-set-key (kbd "s-k") #'kill-this-buffer)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-t") #'jump-to-scratch-buffer)
(global-set-key (kbd "s-u") #'universal-argument)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "s-w") #'evil-window-delete)
(global-set-key (kbd "s-z") #'undo)


(set-face-attribute 'fixed-pitch nil :font "Monolisa" :height 140)
(set-face-attribute 'variable-pitch nil :height 160)


(column-number-mode)
(blink-cursor-mode 0)
(show-paren-mode)
(electric-pair-mode 1)
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode 0)))
(save-place-mode t)
(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(add-hook 'after-init-hook #'savehist-mode)


;;; Custom functions


(defun jump-to-scratch-buffer ()
  "Jump to the existing *scratch* buffer or create a new
one. Repeating this command will prompt the user for the name
used to create a new scratch buffer."
  (interactive)
  (if (eq last-command this-command)
      (let ((new-buffer (generate-new-buffer
                         (format
                          "*scratch*<%s>"
                          (read-string
                           "New scratch buffer name for *scratch*<NAME>: ")))))
        (switch-to-buffer new-buffer))
    (if-let (existing-scratch-buffer (get-buffer "*scratch*"))
        (switch-to-buffer existing-scratch-buffer)
      (let ((new-scratch-buffer (get-buffer-create "*scratch*")))
        (switch-to-buffer new-scratch-buffer)))))


(defun dawran/find-config ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el"))
  (add-to-list 'imenu-generic-expression
               '("Packages" "^(use-package\\s-+\\(.+\\)" 1)))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


(defun dawran/load-theme-action (theme)
  "Disable current themes and load theme THEME."
  (progn
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)))


(defun dawran/load-theme ()
  "Disable current themes and load theme from the completion list."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes)))))
    (dawran/load-theme-action theme)))


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


(defun dawran/quick-edit ()
  "Util function for use with hammerspoon quick edit functionality."
  (interactive)
  (let ((qed-buffer (generate-new-buffer "*quick-edit*")))
    (switch-to-buffer qed-buffer)
    (evil-paste-after 1)
    (gfm-mode)))


(defun dawran/quick-edit-end ()
  "Util function to be executed on qed completion."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (kill-current-buffer))


;;; - Mac


(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'super
        mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier 'meta
        insert-directory-program "gls"
        dired-listing-switches "-AFhlv --group-directories-first")

  ;; Detect system theme
  (when (fboundp 'mac-application-state)
    (add-hook
     'after-init-hook
     (lambda ()
       (load-theme
        (if (equal "NSAppearanceNameDarkAqua"
                   (plist-get (mac-application-state) :appearance))
            'sketch-black
          'sketch-white)
        t))))

  ;; Ligature
  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode)))


;;; - Package manager


(setq straight-build-dir (format "build-%s" emacs-version)
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
;; (setq use-package-verbose t)
;; (setq use-package-compute-statistics t)
(setq use-package-expand-minimally t)


;;; - Built-in Packages


;; ibuffer
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("default"
         ("Scratch" (name . "*scratch*"))
         ("Eww"   (mode . eww-mode))
         ("Kira" (filename . "/kira/"))
         ("Projects" (filename . "/projects/"))
         ("Dired" (mode . dired-mode))
         ("Meta"  (name . "\\*"))
         ("Emacs Config" (filename . ".emacs.d"))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*Info\*"))))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))


(use-package dired
  :hook (;; (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode)
  (setq-default dired-omit-files-p t)
  (require 'dired-x)
  (add-to-list 'dired-omit-extensions ".DS_Store"))


(use-package find-dired
  :commands find-name-dired
  :custom
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))


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
  :hook (eshell-first-time-mode . dawran/configure-eshell))


(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t))


(use-package project
  :preface
  ;; although emacs ships with project.el, the latest version project.el is
  ;; nicer.
  (when (version< emacs-version "28")
    (straight-use-package 'project))
  :bind
  (("s-p" . project-find-file)
   :map project-prefix-map
   ("m" . magit-project-status))
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit")))


(use-package compile
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


;;; - 3rd Party Packages


(use-package general
  :straight t
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "d" #'dired-jump
   "e" #'eshell
   "fd" #'dawran/find-config
   "tc" #'display-time-world
   "tt" #'dawran/load-theme
   "tw" #'whitespace-mode
   "tm" #'toggle-frame-maximized
   "tM" #'toggle-frame-fullscreen))


(use-package evil
  :straight t
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

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd ",") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)

  ;; https://blog.meain.io/2020/emacs-highlight-yanked/
  (defun dawran/evil-yank-advice (orig-fn beg end &rest args)
    "Pulse momentary on yank text"
    (pulse-momentary-highlight-region beg end)
    (apply orig-fn beg end args))
  (advice-add 'evil-yank :around #'dawran/evil-yank-advice))


(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))


(use-package idle-highlight-mode
  :straight t
  :custom-face
  (idle-highlight ((t (:inherit lazy-highlight))))
  :hook
  (prog-mode . idle-highlight-mode))


(use-package sketch-themes
  :defer t
  :straight (:host github :repo "dawranliou/sketch-themes"))


(use-package paren-face
  :straight t
  :hook
  (emacs-lisp-mode . paren-face-mode))


(use-package display-fill-column-indicator
  :straight t
  :hook (prog-mode . display-fill-column-indicator-mode)
  :config
  (diminish 'auto-fill-function))


(use-package ns-auto-titlebar
  :straight t
  :hook (after-init . ns-auto-titlebar-mode))


(use-package rainbow-mode
  :straight t
  :commands rainbow-mode)


(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion))))
  :init
  (setq completion-category-defaults nil))


(use-package corfu
  :straight t
  :init
  (corfu-global-mode))


(use-package vertico
  :straight t
  :init
  (vertico-mode))


(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-light
                                marginalia-annotators-heavy)))


(use-package ctrlf
  :straight (:host github :repo "dawranliou/ctrlf")
  :bind
  ("s-f" . ctrlf-forward-fuzzy)
  :init
  (ctrlf-mode +1)
  :general
  (:states '(motion)
           "*" 'ctrlf-forward-symbol-at-point
           "#" 'ctrlf-forward-symbol-at-point))


(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("s-," . xref-pop-marker-stack)
   ("s-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))


(use-package helpful
  :straight t
  :defer t
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-key]      . #'helpful-key)
         ([remap describe-symbol]   . #'helpful-symbol)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h C"   . #'helpful-command)
         ("C-h F"   . #'describe-face)))


(use-package persistent-scratch
  :straight t
  :custom
  (persistent-scratch-autosave-interval 60)
  :config
  (persistent-scratch-setup-default))


(use-package ws-butler
  :straight t
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode))
  :custom
  (ws-butler-keep-whitespace-before-point nil)
  :config
  (diminish 'ws-butler-mode))


(use-package paredit
  :straight t
  :hook (emacs-lisp-mode . enable-paredit-mode)
  :bind
  (:map paredit-mode-map
        ("M-s" . nil)
        ("C-M-s" . paredit-splice-sexp)
        ("M-?" . nil))
  :config
  (diminish 'paredit-mode))


(use-package iedit
  :straight t
  :custom
  (iedit-toggle-key-default nil)
  :bind
  (:map
   evil-visual-state-map
   ("R" . iedit-mode)
   :map iedit-mode-occurrence-keymap
   ("RET" . iedit-toggle-selection)
   :map iedit-mode-keymap
   ("C-n" . iedit-next-occurrence)
   ("C-p" . iedit-prev-occurrence)
   ([remap keyboard-escape-quit] . iedit--quit)
   ([remap keyboard-quit] . iedit--quit)
   ([remap evil-force-normal-state] . iedit--quit)))


(use-package undo-fu
  :straight t
  :defer t)


(use-package expand-region
  :straight t
  :bind
  ("s-'" .  er/expand-region)
  ("s-\"" .  er/contract-region)
  :hook
  (prog-mode . dawran/greedy-expansion-list)
  :config
  (defun dawran/greedy-expansion-list ()
    "Skip marking words or inside quotes and pairs"
    (setq-local er/try-expand-list
                (cl-set-difference er/try-expand-list
                                   '(er/mark-word
                                     er/mark-inside-quotes
                                     er/mark-inside-pairs)))))


(defun dawran/org-mode-setup ()
  (setq-local evil-auto-indent nil)
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))


(use-package org
  :straight t
  :hook ((org-mode . dawran/org-mode-setup)
         (org-mode . visual-line-mode)
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
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))


(use-package evil-org
  :straight t
  :after evil
  :hook (org-mode . evil-org-mode)
  :config
  (diminish 'evil-org-mode))


(use-package org-journal
  :straight t
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "nj" #'org-journal-open-current-journal-file
   "nJ" #'org-journal-new-entry)
  :custom
  (org-journal-date-format "%A, %d/%m/%Y")
  (org-journal-date-prefix "* ")
  (org-journal-file-format "%F.org")
  (org-journal-dir "~/org/journal/")
  (org-journal-file-type 'weekly)
  (org-journal-find-file #'find-file))


(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/org/roam/")
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "nf" #'org-roam-node-find
   "nl" #'org-roam-buffer-toggle
   "ng" #'org-roam-graph
   "ni" #'org-roam-node-insert
   "nc" #'org-roam-capture)
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup))


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


(use-package eshell-toggle
  :straight t
  :custom
  (eshell-toggle-use-git-root t)
  (eshell-toggle-run-command nil)
  :bind
  ("C-M-'" . eshell-toggle)
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "te" #'eshell-toggle))


(use-package magit
  :straight t
  :bind (("s-g" . magit-status)
         ("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :commands (magit-project-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "gg"  #'magit-status
   "gb"  #'magit-blame-addition
   "gd"  #'magit-diff-unstaged
   "gf"  #'magit-file-dispatch
   "gl"  #'magit-log-buffer-file))


(use-package rg
  :straight t
  :bind ("s-F" . rg-project)
  :config
  (rg-enable-default-bindings))


(use-package flycheck
  :straight t
  :ensure t
  :hook (prog-init . flycheck-mode))


(use-package lsp-mode
  :straight t
  :defer t
  :hook (lsp-mode . (lambda () (setq-local idle-highlight-mode nil)))
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


(use-package extras
  :load-path "lisp/"
  :bind
  (("M-y" . yank-pop+)
   ("C-x C-r" . recentf-open-files+)))


;;; - Language major modes


(use-package clojure-mode
  :straight t
  :defer t
  :hook
  ((clojure-mode . enable-paredit-mode)
   (clojurescript-mode . enable-paredit-mode)
   (cider-repl-mode . enable-paredit-mode)
   (clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp))
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local hippie-expand-try-functions-list
                          '(try-expand-dabbrev
                            try-expand-dabbrev-all-buffers
                            try-expand-dabbrev-from-kill))))
  :custom
  (cljr-magic-requires nil)
  :config
  ;; (require 'flycheck-clj-kondo)
  (setq clojure-indent-style 'align-arguments
        clojure-align-forms-automatically t)
  (with-eval-after-load 'clj-refactor
    (diminish 'clj-refactor-mode)))


(use-package cider
  :straight t
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  :hook
  (cider-repl-mode . evil-insert-state)
  :bind
  (:map cider-mode-map
        ("M-," . nil)                   ; Prefer xref + clojure-lsp
        ("M-." . nil))                  ; Prefer xref + clojure-lsp
  :general
  (general-define-key
   :state 'normal
   :prefix ","
   :keymaps '(clojure-mode-map clojurescript-mode-map)
   "," #'cider
   "eb" #'cider-eval-buffer
   "ef" #'cider-eval-defun-at-point
   "eF" #'cider-pprint-eval-defun-to-comment
   "ee" #'cider-eval-last-sexp
   "eE" #'cider-pprint-eval-last-sexp-to-comment
   "tt" #'cider-test-run-test
   "tn" #'cider-test-run-ns-tests))


(use-package go-mode
  :straight t
  :mode "\\.go\\'")


(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :hook ((markdown-mode . auto-fill-mode))
  :config
  (setq markdown-command "marked"))


(use-package emmet-mode
  :straight t
  :hook
  (html-mode . emmet-mode)
  (css-mode . emmet-mode))


(use-package yaml-mode
  :straight t
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")


(use-package fennel-mode
  :straight (:host gitlab :repo "technomancy/fennel-mode")
  :mode "\\.fnl\\'"
  :hook (fennel-mode . lispy-mode))


(use-package flyspell
  :bind
  (:map flyspell-mode-map
        ("C-," . nil)
        ("C-;" . nil))
  :hook
  ;; (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode))


(use-package slime
  :straight t
  :commands slime
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (load (expand-file-name "~/.quicklisp/slime-helper.el")))


;;; - Elfeed


(use-package elfeed
  :straight t
  :custom
  (elfeed-feeds '("https://dawranliou.com/atom.xml"
                  "https://ruzkuku.com/all.atom"
                  "https://ambrevar.xyz/atom.xml"
                  "https://erick.navarro.io/index.xml"
                  "https://endlessparentheses.com/atom.xml"
                  "https://www.murilopereira.com/index.xml"
                  "https://drewdevault.com/blog/index.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://blog.meain.io/feed.xml"
                  "https://technomancy.us/atom.xml"
                  "https://worace.works/atom.xml"
                  "https://clojure.org/feed.xml"
                  "http://irreal.org/blog/?feed=rss2"
                  "https://emacsredux.com/atom.xml"))
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "R" #'elfeed))


;;; - EWW


(setq shr-use-colors nil
      shr-use-fonts nil
      shr-indentation 0
      shr-max-image-proportion 0.5
      shr-image-animate nil
      shr-width 72
      shr-discard-aria-hidden t
      shr-cookie-policy nil)

(use-package elpher
  :straight t
  :commands elpher)

(setq ediff-window-setup-function #'ediff-setup-windows-plain)
(setq ediff-split-window-function #'split-window-horizontally)


;;; - ERC

(setq erc-server "irc.libera.chat"
      erc-nick "dawranliou"
      erc-user-full-name "Daw-Ran Liou"
      erc-track-shorten-start 8
      erc-kill-buffer-on-part t
      ;; erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      ;; erc-auto-query 'bury
      )

(provide 'init)

;;; init.el ends here
