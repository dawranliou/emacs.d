;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;;

;;; Code:


;;; - Performance


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 100 1024 1024) ; 100mb
         gc-cons-percentage 0.1)
   (message "*** Emacs loaded in %.2f seconds with %d garbage collections."
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)
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

      ;; backups
      make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      backup-directory-alist
      (list (cons "." (expand-file-name "var/backup/" user-emacs-directory)))
      auto-save-list-file-prefix
      (expand-file-name "var/auto-save/" user-emacs-directory)
      ring-bell-function #'ignore
      visible-bell nil
      ns-use-proxy-icon nil
      frame-title-format nil
      enable-recursive-minibuffers t
      recentf-save-file
      (expand-file-name "var/recentf-save.el" user-emacs-directory)
      recentf-max-saved-items 200
      savehist-file (expand-file-name "var/savehist.el" user-emacs-directory)
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
      hippie-expand-try-functions-list '(try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill)
      custom-file (expand-file-name "custom.el" user-emacs-directory))


(load custom-file 'noerror)


(setq-default delete-by-moving-to-trash t
              scroll-preserve-screen-position 'always
              fill-column 80
              cursor-type '(bar . 2)
              x-stretch-cursor t
              tab-width 8
              tab-always-indent 'complete
              line-spacing 3
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
                                 ;; (vc-mode vc-mode)
                                 "  "
                                 mode-line-modes
                                 mode-line-misc-info
                                 mode-line-end-spaces))


(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(global-set-key (kbd "C-M-j") #'switch-to-buffer)
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'delete-other-windows)
(global-set-key (kbd "M-j") #'join-line)
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "M-:") #'pp-eval-expression)
(global-set-key (kbd "M-Z") #'zap-to-char)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "s--") #'text-scale-decrease)
(global-set-key (kbd "s-<backspace>") #'kill-whole-line)
(global-set-key (kbd "s-=") #'text-scale-adjust)
(global-set-key (kbd "s-S") #'write-file)
(global-set-key (kbd "s-a") #'mark-whole-buffer)
(global-set-key (kbd "s-i") #'imenu)
(global-set-key (kbd "s-k") #'kill-this-buffer)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-t") #'jump-to-scratch-buffer)
(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-v") #'yank)
(global-set-key (kbd "C-c f d") #'+find-config)
(global-set-key (kbd "C-c t t") #'+load-theme)
(global-set-key (kbd "C-c t w") #'whitespace-mode)
(global-set-key (kbd "C-c t m") #'toggle-frame-maximized)
(global-set-key (kbd "C-c t M") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c t $") #'toggle-truncate-lines)


(custom-set-faces
 '(fixed-pitch ((t (:inherit default :height 140))))
 '(variable-pitch ((t (:height 160)))))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


(column-number-mode)
(show-paren-mode)
(electric-pair-mode 1)
(save-place-mode t)
(add-hook 'after-init-hook #'savehist-mode)
(cua-selection-mode 1)


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


(defun +find-config ()
  (interactive)
  (find-file (expand-file-name user-init-file)))


(defun +load-theme-action (theme)
  "Disable current themes and load theme THEME."
  (progn
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)))


(defun +load-theme ()
  "Disable current themes and load theme from the completion list."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes)))))
    (+load-theme-action theme)))


;; Inspired by diminish.el
;; https://github.com/myrjola/diminish.el/blob/master/diminish.el
(defun diminish (mode)
  "Diminish minor mode MODE."
  (let ((minor (assq mode minor-mode-alist)))
    (when minor
      (setcdr minor (list "")))))


(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))


(defun +quick-edit ()
  "Util function for use with hammerspoon quick edit functionality."
  (interactive)
  (let ((qed-buffer (generate-new-buffer "*quick-edit*")))
    (switch-to-buffer qed-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (gfm-mode)))


(defun +quick-edit-end ()
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
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aFGhlv --group-directories-first")

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
        t)))))


;;; - Package manager


(setq straight-build-dir (format "build-%s" emacs-version)
      ;; Lazy modification detection speeds up the startup time. I don't often
      ;; modify packages anyway. When I do, I can build the package manually, I
      ;; think.
      straight-check-for-modifications '(check-on-save find-when-checking))

(load (expand-file-name
       "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)

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
  (require 'dired-x)
  (add-to-list 'dired-omit-extensions ".DS_Store"))


(use-package find-dired
  :commands find-name-dired
  :custom
  (find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))


(defun +eshell-history ()
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


(defun +configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use Ivy to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  (define-key eshell-mode-map (kbd "C-r") '+eshell-history)
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)

  (setq eshell-history-size          10000
        eshell-buffer-maximum-lines  10000
        eshell-hist-ignoredups           t
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t))


(use-package eshell
  :hook (eshell-first-time-mode . +configure-eshell))


(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t))


(use-package project
  :preface
  ;; although emacs ships with project.el, the latest version project.el is
  ;; nicer.
  (when (version< emacs-version "28")
    (straight-use-package 'project))
  :custom
  (project-list-file (expand-file-name "var/projects.el" user-emacs-directory))
  :bind
  ("s-p" . project-find-file)
  (:map project-prefix-map
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
  :mode ("\\.log\\'" . auto-revert-tail-mode)
  :config
  (diminish 'auto-revert-mode))


(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (search-whitespace-regexp ".*?")      ; Fuzzy search
  :bind
  ("s-f" . isearch-forward)
  (:map isearch-mode-map
        ("C-o" . isearch-occur)
        ("<C-backspace>" . isearch-delete-wrong)
        ;; DEL during isearch should edit the search string, not jump back to
        ;; the previous result
        ([remap isearch-delete-char] . isearch-del-char))
  :hook
  (isearch-mode-end . isearch-exit-at-start)
  :config
  ;; https://www.emacswiki.org/emacs/IncrementalSearch#h5o-4
  (defun isearch-exit-at-start ()
    "Exit search at the beginning of the current match."
    (when (and isearch-forward
               isearch-other-end
               (not isearch-mode-end-hook-quit))
      (goto-char isearch-other-end)))
  (defun isearch-delete-wrong ()
    "Revert to previous successful search."
    (interactive)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state))
    (isearch-update)))


;;; - My extras lisp


(use-package extras
  :load-path "lisp/"
  :commands (+uuid)
  :bind
  (([remap move-beginning-of-line] . +move-beginning-of-line)
   ("C-<backspace>" . +kill-line-backwards)
   ("S-<return>" . +newline-at-end-of-line)
   ("M-y" . yank-pop+)
   ("C-x C-r" . recentf-open-files+)
   ("C-M-'" . +eshell-here)
   ("C-w" . #'+backward-kill-word-or-region)))


;;; - 3rd Party Packages


(use-package modalka
  :straight t
  :custom
  (modalka-cursor-type 'box)
  :bind
  (:map esc-map
        ("<escape>" . modalka-global-mode))
  (:map modalka-mode-map
        ([remap self-insert-command] . 'ignore))
  :config
  (modalka-define-kbd "0" "C-0")
  (modalka-define-kbd "1" "C-1")
  (modalka-define-kbd "2" "C-2")
  (modalka-define-kbd "3" "C-3")
  (modalka-define-kbd "4" "C-4")
  (modalka-define-kbd "5" "C-5")
  (modalka-define-kbd "6" "C-6")
  (modalka-define-kbd "7" "C-7")
  (modalka-define-kbd "8" "C-8")
  (modalka-define-kbd "9" "C-9")

  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-b")
  ;; c
  (modalka-define-kbd "d" "C-d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-f")
  (modalka-define-kbd "g" "C-g")
  (modalka-define-kbd "h" "C-b")
  (define-key modalka-mode-map "i" #'modalka-global-mode)
  (modalka-define-kbd "j" "C-n")
  (modalka-define-kbd "k" "C-p")
  (modalka-define-kbd "l" "C-f")
  ;; m
  (modalka-define-kbd "n" "C-n")
  (modalka-define-kbd "o" "S-<return>")
  (modalka-define-kbd "p" "C-p")
  ;; q
  (modalka-define-kbd "r" "C-r")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "t" "C-t")
  ;; u
  (modalka-define-kbd "v" "C-v")
  (modalka-define-kbd "w" "C-w")
  ;; x
  (modalka-define-kbd "y" "C-y")
  ;; z

  (modalka-define-kbd "A" "C-M-a")
  (modalka-define-kbd "B" "C-M-b")
  (modalka-define-kbd "D" "C-M-d")
  (modalka-define-kbd "E" "C-M-e")
  (modalka-define-kbd "F" "C-M-f")
  (modalka-define-kbd "T" "C-M-t")
  (modalka-define-kbd "U" "C-M-u")
  (modalka-define-kbd "K" "C-M-k")
  )


(use-package sketch-themes
  :defer t
  :straight (:host github :repo "dawranliou/sketch-themes"))


(use-package paren-face
  :straight t
  :hook
  (emacs-lisp-mode . paren-face-mode))


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


(use-package selectrum
  :straight t
  :bind
  ("C-x C-z" . #'selectrum-repeat)
  :custom
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :init
  (selectrum-mode +1))


(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-light
                                marginalia-annotators-heavy)))


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
  :custom (helpful-switch-buffer-function #'+helpful-switch-to-buffer)
  :bind (;; Remap standard commands.
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-key]      . #'helpful-key)
         ([remap describe-symbol]   . #'helpful-symbol)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h C"   . #'helpful-command)
         ("C-h F"   . #'describe-face))
  :config
  ;; https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun +helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))))


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
        ("M-r" . nil)
        ("C-M-r" . paredit-raise-sexp)
        ("]" . nil)
        ("}" . nil)
        (")" . nil)
        ("C-d" . nil)
        ("C-k" . nil)
        ("C->" . paredit-forward-slurp-sexp)
        ("C-<" . paredit-forward-barf-sexp)
        ("C-M->" . paredit-backward-barf-sexp)
        ("C-M-<" . paredit-backward-slurp-sexp)
        ("M-d" . nil)
        ("DEL" . nil)
        ("M-DEL" . nil)
        ("M-?" . nil)
        ("M-{" . paredit-wrap-curly)
        ("M-[" . paredit-wrap-square))
  :config
  (diminish 'paredit-mode))


(use-package iedit
  :straight t
  :bind
  ("C-;" . iedit-mode)
  (:map iedit-mode-keymap
        ("C-n" . iedit-next-occurrence)
        ("C-p" . iedit-prev-occurrence)))


(use-package undo-fu
  :straight t
  :defer t)


(use-package expand-region
  :straight t
  :bind
  ("s-'" .  er/expand-region)
  ("s-\"" .  er/contract-region)
  :hook
  (prog-mode . +greedy-expansion-list)
  :config
  (defun +greedy-expansion-list ()
    "Skip marking words or inside quotes and pairs"
    (setq-local er/try-expand-list
                (cl-set-difference er/try-expand-list
                                   '(er/mark-word
                                     er/mark-inside-quotes
                                     er/mark-inside-pairs)))))


(defun +org-mode-setup ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))


(use-package org
  :straight t
  :hook ((org-mode . +org-mode-setup)
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
  (org-attach-auto-tag "attachment")
  :config
  (require 'ox-md)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))


(use-package org-journal
  :straight t
  :commands org-journal-new-entry
  :bind ("C-c n j" . 'org-journal-new-entry)
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
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup))


(defvar org-paste-clipboard-image-dir "img")


(defun +org-paste-clipboard-image ()
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
  (define-key org-mode-map (kbd "s-y") #'+org-paste-clipboard-image))


(use-package magit
  :straight t
  :bind (("s-g" . magit-status)
         ("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch))
  :commands (magit-project-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))


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
  :custom-face
  (lsp-face-highlight-textual ((t (:inherit lazy-highlight))))
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "s-l")
  (lsp-enable-indentation nil)
  (lsp-completion-provider :none)
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-scope :file)
  (lsp-modeline-code-actions-enable nil)
  :config
  (setq-default read-process-output-max (* 1024 1024)))


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
  (defun +clojure-ns-kill-ring-save ()
    "Save the current clojure ns to the kill ring."
    (interactive)
    (let ((ns (funcall clojure-expected-ns-function)))
      (kill-new ns)
      (message (format "Saved to kill-ring: %s" ns)))))


(use-package cider
  :straight t
  :after clojure-mode
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-display-in-current-window nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-pretty-printing t)
  (cider-repl-buffer-size-limit 100000)
  :bind
  (:map cider-mode-map
        ("M-," . nil)
        ("M-." . nil)))


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
  :hook (fennel-mode . paredit-mode))


(use-package flyspell
  :bind
  (:map flyspell-mode-map
        ("C-." . nil)
        ("C-," . nil)
        ("C-;" . nil))
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :config
  (diminish 'flyspell-mode))


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
  :commands elfeed
  :custom
  (elfeed-feeds '("http://irreal.org/blog/?feed=rss2"
                  "https://ambrevar.xyz/atom.xml"
                  "https://blog.meain.io/feed.xml"
                  "https://clojure.org/feed.xml"
                  "https://d12frosted.io/atom.xml"
                  "https://dawranliou.com/atom.xml"
                  "https://drewdevault.com/blog/index.xml"
                  "https://emacsredux.com/atom.xml"
                  "https://endlessparentheses.com/atom.xml"
                  "https://erick.navarro.io/index.xml"
                  "https://protesilaos.com/codelog.xml"
                  "https://ruzkuku.com/all.atom"
                  "https://technomancy.us/atom.xml"
                  "https://worace.works/atom.xml"
                  "https://www.manueluberti.eu/feed.xml"
                  "https://www.murilopereira.com/index.xml"
                  "https://www.with-emacs.com/rss.")))


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


(provide 'init)

;;; init.el ends here
