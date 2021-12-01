;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;;

;;; Code:

;;; Package Management

;; The very first thing I do is setup the packages I need. I do this so that
;; when I open this config on a new machine, all the packages needed to make it
;; work are specified in `package-selected-packages', with that one you can
;; install them with `package-install-selected-packages'. At which point the
;; whole config should be ready to rock.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities '(("gnu" . 30)
                                   ("nongnu" . 25)
                                   ("melpa-stable" . 20)
                                   ("melpa" . 10)))

(setq package-selected-packages
      '(avy
        cider
        clojure-mode
        elfeed
        elpher
        embark
        emmet-mode
        fennel-mode
        flycheck
        flyspell
        go-mode
        helpful
        iedit
        lsp-mode
        magit
        marginalia
        markdown-mode
        orderless
        org
        org-journal
        org-roam
        persistent-scratch
        rainbow-mode
        rg
        selectrum
        sketch-themes
        slime
        smartscan
        sqlformat
        ws-butler
        yaml-mode))


(defmacro elpa-package (package &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent defun))
  `(if (package-installed-p ,package)
       (progn ,@body)
     (message (concat "Package \'"
                      (symbol-name ,package)
                      "\' is not installed... skipping config."))))


;;; Settings

;; Save all interactive customization to a temp file, which is never loaded.
;; This means interactive customization is session-local. Only this init file
;; persists sessions.

(setq custom-file (make-temp-file "emacs-custom-"))

(setq
 inhibit-startup-message t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 tramp-default-method "ssh"
 delete-by-moving-to-trash t

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
 ;; frame-title-format nil
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
 scroll-conservatively 101              ; Don't recenter
 ediff-window-setup-function #'ediff-setup-windows-plain
 ediff-split-window-function #'split-window-horizontally
 shr-use-colors nil
 shr-use-fonts nil
 shr-indentation 0
 shr-max-image-proportion 0.5
 shr-image-animate nil
 shr-width 72
 shr-discard-aria-hidden t
 shr-cookie-policy nil

 ;; Emacs 29
 show-paren-context-when-offscreen t)




(setq-default
 fill-column 80
 x-stretch-cursor t
 tab-width 8
 tab-always-indent t
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
(fset 'yes-or-no-p 'y-or-n-p)


(global-set-key (kbd "C-M-j") #'switch-to-buffer)
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-M-r") #'raise-sexp)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-i") #'delete-other-windows)
(global-set-key (kbd "M-SPC") #'cycle-spacing)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "M-Z") #'zap-to-char)
(global-set-key (kbd "M-z") #'zap-up-to-char)
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
(global-set-key (kbd "C-\\") #'undo-only)
(global-set-key (kbd "C-h p") #'describe-package)  ; Swap the two
(global-set-key (kbd "C-h P") #'finder-by-keyword)
(global-set-key (kbd "C-h L") #'find-library)
(global-set-key [remap eval-expression] #'pp-eval-expression) ; M-:
(global-set-key [remap eval-last-sexp] #'pp-eval-last-sexp)   ; C-x C-e


(custom-set-faces
 '(fixed-pitch ((t (:inherit default :height 140))))
 '(variable-pitch ((t (:height 160)))))


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


(column-number-mode)
(show-paren-mode)
(electric-pair-mode 1)
(save-place-mode t)
(add-hook 'after-init-hook #'savehist-mode)
(winner-mode)
(global-so-long-mode)
(delete-selection-mode)


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
        trash-directory "~/.Trash"
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aFGhlv --group-directories-first"))


;;; - Theme


(add-hook
 'after-init-hook
 (lambda ()
   (load-theme
    ;; Try to detect system theme
    (if (and (fboundp 'mac-application-state)
             (equal "NSAppearanceNameDarkAqua"
                    (plist-get (mac-application-state) :appearance)))
        'sketch-black
      'sketch-white)
    t)))


;;; - Built-in Packages


(with-eval-after-load 'dired
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (define-key global-map (kbd "C-x C-j") 'dired-jump)
  (custom-set-variables
   '(dired-auto-revert-buffer t)
   '(dired-dwim-target t)
   '(dired-recursive-copies 'always)
   '(dired-recursive-deletes 'always))
  (require 'dired-x)
  (add-to-list 'dired-omit-extensions ".DS_Store"))


(with-eval-after-load 'find-dired
  (custom-set-variables
   '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))))


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


(with-eval-after-load 'eshell
  (add-hook 'eshell-first-time-mode-hook '+configure-eshell))


(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t))


(with-eval-after-load 'project
  (custom-set-variables
   '(project-list-file (expand-file-name "var/projects.el" user-emacs-directory)))
  (define-key global-map 'project-find-file)
  ;; Setup the `project-switch-commands'
  (require 'magit-extras))


(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))


(add-to-list 'auto-mode-alist
             '("\\.log\\'" . auto-revert-tail-mode))


(with-eval-after-load 'isearch
  (custom-set-variables
   '(isearch-allow-scroll t)
   '(search-whitespace-regexp ".*?")      ; Fuzzy search
   ;; Emacs 28
   '(isearch-allow-motion t)
   '(isearch-wrap-pause 'no))

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
    (isearch-update))

  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
  (define-key isearch-mode-map (kbd "<C-backspace>") 'isearch-delete-wrong)
  ;; DEL during isearch should edit the search string, not jump back to the
  ;; previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))


;;; - My extras lisp


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'extras)
(global-set-key (kbd "C-<backspace>") '+kill-line-backwards)
(global-set-key (kbd "C-<backspace>") '+kill-line-backwards)
(global-set-key (kbd "S-<return>") '+newline-at-end-of-line)
(global-set-key (kbd "C-x C-r") 'recentf-open-files+)
(global-set-key (kbd "C-M-'") '+eshell-here)
(global-set-key (kbd "C-w") '+backward-kill-word-or-region)
(global-set-key (kbd "M-Q") '+unfill-paragraph)
(global-set-key (kbd "M-q") '+fill-or-unfill-paragraph)
(define-key ctl-x-4-map (kbd "s") '+toggle-window-split)
(define-key ctl-x-4-map (kbd "t") '+transpose-windows)


(add-to-list 'load-path (expand-file-name "site-lisp/"))
(with-eval-after-load 'smartscan
  (add-hook 'prog-mode-hook 'smartscan-mode))


;;; - 3rd Party Packages


(elpa-package 'sketch-themes)


(elpa-package 'rainbow-mode
  (unless
      (fboundp 'rainbow-mode)
    (autoload #'rainbow-mode "rainbow-mode" nil t)))


(elpa-package 'orderless
  (custom-set-variables
   '(completion-styles '(orderless))
   '(completion-category-overrides '((file (styles partial-completion)))))
  (setq completion-category-defaults nil))


(elpa-package 'selectrum
  (global-set-key (kbd "C-x C-z") 'selectrum-repeat)
  (custom-set-variables
   '(orderless-skip-highlighting (lambda () selectrum-is-active))
   '(selectrum-highlight-candidates-function #'orderless-highlight-matches))
  (selectrum-mode +1))


(elpa-package 'marginalia
  (define-key minibuffer-local-map (kbd "C-M-a") 'marginalia-cycle)
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-light
                                marginalia-annotators-heavy)))


(elpa-package 'embark
  (global-set-key (kbd "s-,") 'xref-pop-marker-stack)
  (global-set-key (kbd "s-.") 'embark-act)
  (global-set-key (kbd "C-h B") 'embark-bindings)
  (setq prefix-help-command #'embark-prefix-help-command))


(elpa-package 'avy
  (global-set-key (kbd "M-j") 'avy-goto-char-timer))


(elpa-package 'helpful
  ;; Remap standard commands.
  (global-set-key [remap describe-function] 'helpful-callable)
  (global-set-key [remap describe-variable] 'helpful-variable)
  (global-set-key [remap describe-key]      'helpful-key)
  (global-set-key [remap describe-symbol]   'helpful-symbol)
  (global-set-key (kbd "C-c C-d") 'helpful-at-point)
  (global-set-key (kbd "C-h C")   'helpful-command)
  (global-set-key (kbd "C-h F")   'describe-face)

  ;; https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun +helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name)))

  (custom-set-variables
   '(helpful-switch-buffer-function #'+helpful-switch-to-buffer)))


(elpa-package 'persistent-scratch
  (custom-set-variables
   '(persistent-scratch-autosave-interval 60))
  (persistent-scratch-setup-default))


(elpa-package 'ws-butler
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (custom-set-variables
   '(ws-butler-keep-whitespace-before-point nil)))


(elpa-package 'iedit
  (global-set-key (kbd "C-;") 'iedit-mode)
  (with-eval-after-load 'iedit-mode
    (define-key iedit-mode-keymap (kbd "C-n") 'iedit-next-occurrence)
    (define-key iedit-mode-keymap (kbd "C-p") 'iedit-prev-occurrence)))


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
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-switchb)
  (:map org-mode-map
        ("C-," . nil))
  :custom
  (org-hide-emphasis-markers t)
  (org-ellipsis " …")
  (org-special-ctrl-a/e t)
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
  (org-startup-folded 'content)
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
  :bind
  (("s-F" . rg-project)
   ("C-c r" . rg))
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
  (lsp-keymap-prefix "C-c L")
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
  ((clojure-mode . lsp)
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
  :defer t)


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
  :mode "\\.fnl\\'")


(use-package flyspell
  :bind
  (:map flyspell-mode-map
        ;; ("C-." . nil)
        ;; ("C-," . nil)
        ("C-;" . nil))
  :hook
  (prog-mode . flyspell-prog-mode)
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


(use-package elpher
  :straight t
  :commands elpher)


(provide 'init)

;;; init.el ends here

;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
