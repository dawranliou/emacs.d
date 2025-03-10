;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;; This config targets Emacs 30

;;; Code:

;; Opt out customization interface
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (load custom-file :no-error-if-file-is-missing)

;;; Package Management
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-native-compile t)

;;; Auto-save and backup
(setq auto-save-include-big-deletions t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/var/auto-save" t)))
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/")
(setq backup-by-copying t)
(setq backup-by-copying-when-linked t)
(setq backup-directory-alist '(("." . "~/.emacs.d/var/backup/")))
(setq create-lockfiles nil)
(setq delete-old-versions t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq kill-buffer-delete-auto-save-files t)
(setq make-backup-files t)
(setq version-control t)

;;; Treesitter

;; https://robbmann.io/posts/emacs-treesit-auto/
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure.git")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (commonlisp "https://github.com/thehamsta/tree-sitter-commonlisp")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (latex "https://github.com/latex-lsp/tree-sitter-latex")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (markdown_inline "https://github.com/MDeiml/tree-sitter-markdown"
                         "v0.1.6"
                         "tree-sitter-markdown-inline/src")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (zig "https://github.com/GrayJack/tree-sitter-zig")))

(setq major-mode-remap-alist
      '(
        ;; (sh-mode . bash-ts-mode)
        ;; (clojurec-mode . clojure-ts-mode)
        ;; (clojurescript-mode . clojure-ts-mode)
        ;; (clojure-mode . clojure-ts-mode)
        ;; (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (go-dot-mod-mode . go-mod-ts-mode)
        ;; (mhtml-mode . html-ts-mode)
        ;; (sgml-mode . html-ts-mode)
        (java-mode . java-ts-mode)
        ;; (js-mode . js-ts-mode)
        ;; (javascript-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        ;; (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(defun install-latest-known-treesitter-grammars ()
  "Install/upgrade all latest Tree-sitter grammars."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (message "Downloading %s treesitter grammar from %s" (car grammar) (cadr grammar))
    (treesit-install-language-grammar (car grammar))))

(defun remove-treesit-sexp-changes ()
  (when (eq forward-sexp-function #'treesit-forward-sexp)
    (setq forward-sexp-function nil))
  (when (eq transpose-sexps-function #'treesit-transpose-sexps)
    (setq transpose-sexps-function #'transpose-sexps-default-function))
  (when (eq forward-sentence-function #'treesit-forward-sentence)
    (setq forward-sentence-function #'forward-sentence-default-function)))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; Keybindings

(keymap-global-set "<f5>" #'eshell-toggle)
(keymap-global-set "C-g" #'keyboard-quit-dwim)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x k" #'kill-current-buffer)
(keymap-global-set "C-x j" #'duplicate-dwim)
(keymap-global-set "C-M-r" #'raise-sexp)
(keymap-global-set "C-M-d" #'down-list)
(keymap-global-set "C-M-<return>" #'newline-at-end-of-line)
(keymap-global-set "C-S-t" #'scratch-buffer)
(keymap-global-set "M-`" #'other-frame)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-i" #'delete-other-windows)
(keymap-global-set "M-SPC" #'cycle-spacing)
(keymap-global-set "M-Z" #'zap-to-char)
(keymap-global-set "<remap> <dabbrev-expand>" #'hippie-expand) ; M-/
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "C-c z" #'compile)
(keymap-global-set "C-c d" #'find-config)
(keymap-global-set "C-c t f" #'display-fill-column-indicator-mode)
(keymap-global-set "C-c t T" #'load-one-theme)
(keymap-global-set "C-c t w" #'whitespace-mode)
(keymap-global-set "C-c t m" #'toggle-frame-maximized)
(keymap-global-set "C-c t M" #'toggle-frame-fullscreen)
(keymap-global-set "C-c t $" #'toggle-truncate-lines)
(keymap-global-set "C-\\" #'undo-only)
(keymap-global-set "C-h p" #'describe-package)  ; Swap the two
(keymap-global-set "C-h P" #'finder-by-keyword)
(keymap-global-set "C-h L" #'find-library)
(keymap-global-set "C-z" nil)
;; (keymap-substitute global-map #'eval-expression #'pp-eval-expression) ; M-:
;; (keymap-substitute global-map #'eval-last-sexp #'pp-eval-last-sexp)   ; C-x C-e
(keymap-set emacs-lisp-mode-map "C-c C-j" #'eval-print-last-sexp)
(keymap-global-set "<remap> <move-beginning-of-line>" 'move-beginning-of-line+) ; C-a
(keymap-global-set "C-c r" #'recentf-open-files+)
(keymap-global-set "C-w" #'backward-kill-word-or-region)
(keymap-global-set "M-q" #'fill-or-unfill) ; M-q
(keymap-set window-prefix-map "S" #'window-toggle-side-windows) ; Was "s" but rarely used
(keymap-set window-prefix-map "s" #'toggle-window-split)
(keymap-set window-prefix-map "t" #'window-swap-states) ; aka transpose windows
(keymap-global-set "s-{" #'tab-previous)
(keymap-global-set "s-}" #'tab-next)

;;; Aliases

(defalias 'save-as-file #'write-file
  "I can never remember the name of the function")

;;; Functions

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun maybe-split-window (&rest r)
  "Split window sensibly if there's only one window."
  (when (one-window-p)
    (split-window-sensibly)))

(advice-add 'other-window :before #'maybe-split-window)

;; Confirm killing modified buffers
;; https://www.olivertaylor.net/emacs/buffer-confirm-kill.html
(defvar-local buffer-confirm-kill nil
  "Non-nil means to confirm killing buffer when modified.
Variable is checked by `buffer-confirm-kill-p'.")

(defun buffer-confirm-kill-p ()
  "Return nil if buffer is modified and `buffer-confirm-kill' is t.
This function is designed to be called from `kill-buffer-query-functions'."
  (if (and (buffer-modified-p)
           buffer-confirm-kill)
      (yes-or-no-p
       (format "Buffer %s modified; kill anyway?" (buffer-name)))
    t))

(add-hook 'kill-buffer-query-functions #'buffer-confirm-kill-p)

(defun find-config ()
  (interactive)
  (find-file (expand-file-name user-init-file)))

(defun load-one-theme ()
  "Disable current themes and load theme from the completion list."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)))

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(defun move-beginning-of-line+ (arg)
  "Move point to beginning of current line or the first non whitespace char."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Move by lines, if ARG is not 1 (the default).
  (if (/= arg 1)
      (let ((line-move-visual nil))
        (line-move (1- arg) t)))

  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))

(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun uuid ()
  "Generate a new UUID and add it to the kill ring."
  (interactive)
  (require 'org-id)
  (-kill-and-echo (org-id-uuid)))

(defun backward-kill-word-or-region (&optional arg)
  "Kill word backwards unless region is active,
kill region instead"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (or arg 1))))

(defun toggle-window-split ()
  "Toggle window split from vertical to horizontal."
  (interactive)
  (unless (= (length (window-list)) 2)
    (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically))
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(defun fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun -kill-and-echo (X)
  "Copy `X' into the `kill-ring' and echo to the minibuffer."
  (kill-new X)
  (message "[COPIED] %s" X))

(defun copy-path ()
  "Echo file name to minibuffer and copy to kill ring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
        (proj (project-current nil)))
    (if proj
        (let* ((proj-path (expand-file-name (cdr proj)))
               (filename* (nth 1 (split-string filename proj-path))))
          (-kill-and-echo filename*))
      (-kill-and-echo filename))))

(defun copy-current-location ()
  "Show the current location and put it into the kill ring.
Use the filename relative to the current VC root directory."
  (interactive)
  (let* ((file-name (file-relative-name buffer-file-name (vc-root-dir)))
         (line-number (line-number-at-pos nil t))
         (location (format "%s:%s" file-name line-number)))
    (-kill-and-echo location)))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression.
https://www.emacswiki.org/emacs/AlignCommands"
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun simple-http-server ()
  "Starts a simple http server."
  (interactive)
  (async-shell-command "python3 -m http.server"
                       (format "*simple http server* [%s]"
                               default-directory))
  (browse-url "http://localhost:8000"))

(defun miniserve ()
  "Starts a miniserve server at `default-directory'."
  (interactive)
  (async-shell-command "miniserve --port 8000 ."
                       (format "*miniserve* [%s]"
                               default-directory))
  (browse-url "http://localhost:8000"))

(defun shuffle (seq)
  (cl-loop for i from (length seq) downto 2
           do (cl-rotatef (elt seq (random i))
                          (elt seq (1- i))))
  seq)

(defun shuffle-lines-in-region (beg end)
  (interactive "r")
  (shell-command-on-region beg end "shuf" t t))

(defun display-ansi-colors (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region or whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

(defun tab-create-or-select (name)
  "Create the NAME tab if it doesn't exist already."
  (require 'dash)
  (if (-first (lambda (tab) (equal name (alist-get 'name tab)))
              (tab-bar-tabs))
      (tab-bar-select-tab-by-name name)
    (tab-new)
    (tab-bar-rename-tab name)))

(defun eshell-toggle (exit)
  "Bring up a full-screen eshell or restore previous config.
With a prefix argument, exit eshell before restoring previous config."
  (interactive "P")
  (if (string= "eshell-mode" major-mode)
      (progn
        (when exit
          (insert "exit")
          (eshell-send-input))
        (jump-to-register ?!))
    (window-configuration-to-register ?!)
    (eshell)
    (delete-other-windows)))

(defun hexify-string (strg)
  (mapconcat (lambda (c) (format "%x" c)) strg ""))

(defun decode-hex-string (hex-string)
  (with-temp-buffer
    (insert-char 32 (/ (length hex-string) 2))
    (beginning-of-buffer)
    (hexl-mode)
    (hexl-insert-hex-string hex-string 1)
    (hexl-mode-exit)
    (buffer-string)))

(defun +get-process-id-by-name (process-name)
  "Return the process ID of a process specified by PROCESS-NAME. Works on Unix-like systems (Linux, MacOS)."
  (interactive)
  (let ((pid nil))
    (cond
     ((memq system-type '(gnu/linux darwin))
      (setq pid (shell-command-to-string
                 (format "pgrep -f %s"
                         (shell-quote-argument process-name)))))
     (t
      (error "Unsupported system type: %s" system-type)))

    ;; Clean up the output and return first PID
    (when (and pid (not (string-empty-p pid)))
      (car (split-string pid "\n" t)))))

;;; Mac

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        trash-directory "~/.Trash"
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aFghlv --group-directories-first"))

;;; UI

(add-hook 'after-init-hook #'context-menu-mode)

(setq cursor-in-non-selected-windows nil)
(setq frame-resize-pixelwise t)

(setq ring-bell-function 'flash-mode-line)
(setq visible-bell nil)

;; Reduce cursor lag by :
;; 1. Prevent automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolve the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(setq fast-but-imprecise-scrolling t)
(setq hscroll-margin 2)
(setq hscroll-step 1)
(setq mouse-wheel-flip-direction t)
(setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll)
                                    ((meta)) ((control meta) . global-text-scale)
                                    ((control) . text-scale)))
(setq mouse-wheel-scroll-amount-horizontal 2)
(setq mouse-wheel-tilt-scroll t)
(setq scroll-conservatively 10000)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)
(setq scroll-step 1)

(setq x-stretch-cursor t)

(setq-default fill-column 80)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq-default word-wrap t)

(setq column-number-mode t)

(setq echo-keystrokes 0.2)
(setq idle-update-delay 1.0)
(setq indicate-empty-lines t)
(setq register-preview-delay 0.5)

;;;; UI - Windows
(add-hook 'after-init-hook #'winner-mode)

(add-hook 'after-init-hook #'window-divider-mode)
(setq window-divider-default-bottom-width 1)
(setq window-divider-default-places t)
(setq window-divider-default-right-width 1)

;;;; UI - Mode line

(use-package time
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format " %a %e %b, %I:%M%p ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil))

(use-package prot-modeline
  :load-path "site-lisp"
  :config
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  prot-modeline-notmuch-indicator
                  "  "
                  prot-modeline-misc-info)))

(defun flash-mode-line ()
  "Flash the modeline on error or warning.
https://macowners.club/posts/custom-functions-4-ui/"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;;;; UI - Title
(setq ns-use-proxy-icon t)
(setq-default frame-title-format '("%n %b - %F"""))

;;;; UI - Minibuffer

(setq enable-recursive-minibuffers t)
(setq history-length 300)

;;;; UI - Parenthesis

(setq blink-matching-paren nil)
(setq show-paren-context-when-offscreen 'overlay)
(setq show-paren-when-point-in-periphery t)
(setq show-paren-when-point-inside-paren t)

;;; UI - paddings

(use-package spacious-padding
  :ensure t
  :defer t
  :hook (after-init . spacious-padding-mode)
  :custom ((spacious-padding-subtle-mode-line `( :mode-line-active 'default
                                                 :mode-line-inactive vertical-border))))

;;; UI - themes

(use-package ef-themes
  :ensure t
  :defer t
  :custom ((ef-themes-mixed-fonts t)
           (ef-themes-variable-pitch-ui t)
           (ef-themes-to-toggle '(ef-duo-light ef-duo-dark))
           ;; (ef-themes-headings nil)
           (ef-themes-headings ; read the manual's entry or the doc string
           '((0 variable-pitch light 1.9)
             (1 variable-pitch light 1.8)
             (2 variable-pitch light 1.7)
             (3 variable-pitch regular 1.6)
             (4 variable-pitch regular 1.5)
             (5 variable-pitch regular 1.4)
             (6 variable-pitch 1.3)     ; absence of weight means `bold'
             (7 variable-pitch 1.2)
             (t variable-pitch 1.1)))
           )
  :bind (("C-c t t" . #'ef-themes-toggle)))

(add-hook 'after-init-hook (lambda () (load-theme 'ef-night :no-confirm-loading)))

;;; Indentation and formatting

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq tab-always-indent 'complete)

;;; File manager

(setq confirm-nonexistent-file-or-buffer nil)
(setq delete-by-moving-to-trash t)
(setq ffap-machine-p-known 'reject)
(setq find-file-suppress-same-file-warnings t)
(setq find-file-visit-truename t)
(setq find-ls-option '("-print0 | xargs -0 gls -alhd" . "-ld"))
(setq kill-do-not-save-duplicates t)
(setq vc-follow-symlinks t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "\"^\\\\*\"")
(setq uniquify-separator "•")

(add-hook 'after-init-hook #'ffap-bindings)
(add-hook 'after-init-hook #'global-so-long-mode)

;;; Grep

(setq grep-find-command '("rg -n -H --no-heading -e ''" . 27))
(setq xref-search-program 'ripgrep)
(setq xref-show-definitions-function 'consult-xref)
(setq wgrep-auto-save-buffer t)

;;; Tramp

(setq tramp-auto-save-directory "~/.emacs.d/var/tramp-auto-save/")
(setq tramp-backup-directory-alist '(("." . "~/.emacs.d/var/backup/")))
(setq tramp-default-method "ssh")

;;; Text editing

(add-hook 'text-mode-hook 'visual-line-mode)

(setq comment-empty-lines t)
(add-hook 'after-init-hook #'electric-pair-mode)


;;; Misc

(setq auth-sources '("~/.authinfo.gpg"))
(setq calendar-mark-holidays-flag t)
(setq next-error-message-highlight t)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 512 1024))  ; 512kb


;;; Built-in Packages

(use-package dired
  :defer t
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom ((dired-auto-revert-buffer t)
           (dired-dwim-target t)
           (dired-recursive-copies 'always)
           (dired-recursive-deletes 'always)))

(use-package project
  :defer t
  :custom ((project-vc-extra-root-markers '(".project")))
  :config
  ;; Setup the `project-switch-commands'
  (require 'magit-extras)

  (defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
    "An advice function that gives project-find-regexp a unique buffer name"
    (require 'xref)
    (let ((xref-buffer-name (format "%s %s" xref-buffer-name (car args))))
      (apply orig-fun args)))

  (advice-add 'project-find-regexp :around
              #'project-find-regexp-with-unique-buffer))

(use-package compile
  :defer t
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom ((compilation-always-kill t)
           (compilation-ask-about-save nil)
           (compilation-scroll-output 'first-error)
           (comint-buffer-maximum-size 2048)
           (comint-prompt-read-only t))
  :config
  (require 'ansi-color)
  (ansi-color-for-comint-mode-filter))

(use-package autorevert
  :defer t
  :hook (after-init . global-auto-revert-mode)
  :custom ((auto-revert-avoid-polling t)
           (auto-revert-check-vc-info t)
           (auto-revert-stop-on-user-input nil)
           (global-auto-revert-non-file-buffers t)
           (revert-without-query '(".")))
  :config
  (add-to-list 'auto-mode-alist
               '("\\.log\\'" . auto-revert-tail-mode)))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom ((save-place-limit 600)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom ((savehist-additional-variables
            '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring))
           (savehist-save-minibuffer-history t)))

(use-package recentf
  :defer t
  :hook (after-init . recentf-mode)
  :custom ((recentf-max-saved-items 300)))

(use-package isearch
  :defer t
  :bind (:map isearch-mode-map
              ("C-o" . isearch-occur)
              ("C-<backspace>" . isearch-delete-wrong)
              ;; DEL during isearch should edit the search string, not jump back
              ;; to the previous result
              ([remap isearch-delete-char] . isearch-del-char))
  :custom ((isearch-allow-motion t)
           (isearch-allow-scroll t)
           (isearch-lazy-count t)
           (isearch-wrap-pause 'no)
           (isearch-yank-on-move t)
           (search-whitespace-regexp ".*?")
           (lazy-count-prefix-format nil)
           (lazy-count-suffix-format " [%s/%s]"))
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
    (isearch-update))

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(use-package rect
  :defer t
  :config
  ;; https://gist.github.com/jdtsmith/bfa2d692c4fbbffe06b558e4bcf9abec
  (cl-loop for (key def) in
           '(("k" kill-rectangle)       ("t" string-rectangle)
             ("o" open-rectangle)       ("w" copy-rectangle-as-kill)
             ("y" yank-rectangle)       ("c" clear-rectangle)
             ("d" delete-rectangle)     ("N" rectangle-number-lines)
             (" " delete-whitespace-rectangle)
             ("=" calc-grab-sum-across) ("+" calc-grab-sum-down)
             ("#" calc-grab-rectangle)  ("n" set-mark-command)
             ("q" (lambda () (interactive) (deactivate-mark)))
             ("?" (lambda () (interactive)
                    (embark-bindings-in-keymap rectangle-mark-mode-map))))
           do (define-key rectangle-mark-mode-map key def)))

(use-package ediff
  :defer t
  :custom ((ediff-split-window-function 'split-window-sensibly)))

(use-package eldoc
  :defer t
  :custom ((eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

;;; 3rd Party Packages
(use-package magit
  :ensure t
  :defer t
  :custom ((magit-diff-refine-hunk 'all)
           (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
           (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 15))))

(use-package which-key
  :defer t
  :hook (after-init . which-key-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom ((completion-auto-help 'always)
           (completion-auto-select 'second-tab)
           (completion-category-overrides '((file (styles partial-completion))))
           (completion-cycle-threshold 1)
           (completion-styles '(orderless basic))
           (completions-detailed t)
           (completions-format 'one-column)
           (completions-group t)
           (completions-max-height 20))
  :config
  (setq completion-category-defaults nil))

(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :config
  (vertico-multiform-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package embark
  :ensure t
  :defer t
  :commands (embark-next-symbol embark-previous-symbol)
  :bind (("C-." . embark-act)
         ("s-." . embark-dwim)
         ("C-h B" . embark-bindings)
         ("M-n" . embark-next-symbol)
         ("M-s n" . embark-next-symbol)
         ("M-p" . embark-previous-symbol)
         ("M-s p" . embark-previous-symbol))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :ensure t
  :defer t
  :custom ((consult-narrow-key "<"))
  :bind (
         ;; Mode specific
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; ctl-x-map
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer) ; was #'switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; was switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; was switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ; was switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark) ; was #'bookmark-jump
         ("C-x p b" . consult-project-buffer) ; was #'project-switch-to-buffer
         ;; other
         ("M-y" . consult-yank-pop) ; was #'yank-pop
         ;; register
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ; was #'abbrev-prefix-mark
         ("C-M-#" . consult-register)
         ;; goto-map
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line) ; was goto-line
         ("M-g M-g" . consult-goto-line) ; was goto-line
         ("M-g o" . consult-outline)     ; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu) ; was imenu
         ("M-g I" . consult-imenu-multi) ; was imenu
         ;; search-map
         ("M-s d" . consult-find) ; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ; was isearch-edit-string
         ("M-s e" . consult-isearch-history) ; was isearch-edit-string
         ("M-s l" . consult-line) ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ; needed by consult-line to detect isearh
         ;; minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ; was #'next-matching-history-element
         ("M-r" . consult-history) ; was #'previous-matching-history-element
         )
  
  :config
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  )

(use-package avy
  :ensure t
  :defer t
  :bind (("M-j" . avy-goto-char-timer))
  :config
  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select. https://github.com/ebittleman/emacs-bedrock
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))
      t))

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0)
    (pulsar-pulse-line))

  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark)
  (setf (alist-get ?e avy-dispatch-alist) #'avy-action-exchange))

(use-package dired-subtree
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-remove))

  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package nerd-icons-completion
  :ensure t
  :defer t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package helpful
  :ensure t
  :defer t
  :custom ((helpful-switch-buffer-function #'helpful-switch-to-buffer))
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ("C-c C-d"                 . helpful-at-point)
         ("C-h C"                   . helpful-command)
         ("C-h F"                   . describe-face))
  :config
  ;; https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))))

(use-package ws-butler
  :ensure t
  :defer t
  :custom ((ws-butler-keep-whitespace-before-point nil))
  :hook ((prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)))

(use-package iedit
  :ensure t
  :defer t
  :bind (("C-;" . iedit-mode)))

;;; Org mode

(defun org-mode-setup ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))

(add-hook 'org-mode-hook #'org-mode-setup)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'toggle-truncate-lines)

(autoload #'org-store-link "org" nil t)
(autoload #'org-agenda "org" nil t)
(autoload #'org-switchb "org" nil t)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c b" #'org-switchb)
(keymap-global-set "C-c g" #'org-clock-goto)
(keymap-global-set "C-c c" #'org-capture)

(use-package org
  :defer t
  :bind (:map org-mode-map
              ("C-," . nil))
  :custom ((org-align-tags nil)
           (org-tags-column 0)
           (org-attach-auto-tag "attachment")
           (org-babel-load-languages
            '((emacs-lisp . t)
              (sql . t)
              (awk . t)
              (shell . t)
              (clojure . t)))
           (org-catch-invisible-edits 'show-and-error)
           (org-clock-clocked-in-display nil)
           (org-clock-clocktable-default-properties '(:maxlevel 4))
           (org-confirm-babel-evaluate nil)
           (org-cycle-hide-block-startup nil)
           (org-cycle-separator-lines 2)
           (org-default-notes-file "~/org/journal/inbox.org")
           (org-directory "~/org")
           (org-ellipsis "…")
           (org-export-with-sub-superscripts '{})
           (org-fontify-done-headline nil)
           (org-goto-interface 'outline-path-completion)
           (org-hide-block-startup nil)
           (org-hide-emphasis-markers nil)
           (org-image-actual-width '(640))
           (org-indirect-buffer-display 'current-window)
           (org-log-done 'time)
           (org-log-into-drawer t)
           (org-modules '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
           (org-outline-path-complete-in-steps nil)
           (org-pretty-entities t)
           (org-refile-allow-creating-parent-nodes 'confirm)
           (org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
           (org-refile-use-outline-path 'file)
           (org-special-ctrl-a/e 'reversed)
           (org-src-fontify-natively t)
           (org-src-preserve-indentation nil)
           (org-src-tab-acts-natively t)
           (org-src-window-setup 'current-window)
           (org-startup-folded 'content)
           (org-capture-templates
            '(("t" "Todo" entry
               (file+olp+datetree "~/org/journal/journal.org")
               "* TODO %?\12SCHEDULED: %t\12" :clock-in t :clock-resume t :tree-type week)
              ("p" "Project" entry
               (file+olp "~/org/journal/journal.org" "Projects")
               (file "~/.emacs.d/org-templates/project.org")
               :clock-in t :clock-resume t)
              ("j" "Journal" entry
               (file+olp+datetree "~/org/journal/journal.org")
               "* %? %^G\12" :clock-in t :clock-keep t :tree-type week)
              ("d" "Daily Review" entry
               (file+olp+datetree "~/org/journal/journal.org")
               (file "~/.emacs.d/org-templates/daily-review.org")
               :immediate-finish t :clock-in t :clock-keep t :tree-type week)
              ("i" "Check In" entry
               (file+olp+datetree "~/org/journal/journal.org")
               (file "~/.emacs.d/org-templates/check-in.org")
               :immediate-finish t :clock-in t :clock-keep t :tree-type week)
              ("m" "Meeting" entry
               (file+olp+datetree "~/org/journal/journal.org")
               "* %^{Meeting} :meeting:%^G\12" :immediate-finish t :clock-in t :clock-keep t :tree-type week)))
           (org-todo-keyword-faces
            '(("NEXT" . "blue")
              ("REVIEW" . "dark orange")
              ("HOLD" . "purple")
              ("CANCELLED" . "teal")))
           (org-todo-keywords
            '((sequence "TODO(t@/)" "NEXT(n)" "REVIEW(r@/!)" "|" "DONE(d!)")
              (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
           (org-use-speed-commands t))
  :config
  (require 'ox-md)
  (require 'org-tempo)
  (require 'org-habit)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-agenda
  :defer t
  :custom ((org-agenda-files '("~/org/journal/journal.org"))
           (org-agenda-span 'day)
           (org-agenda-start-with-log-mode t)
           (org-agenda-tags-column 0)
           (org-agenda-window-setup 'current-window)
           (org-agenda-time-grid '((daily today require-timed)
                                   (600 1600)
                                   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
           (org-agenda-current-time-string
            "◀── now ─────────────────────────────────────────────────")))

(use-package org-modern
  :ensure t
  :defer t
  :custom ((org-modern-timestamp nil))
  ;; :hook ((org-mode . org-modern-mode)
  ;;        (org-agenda-finalize . org-modern-agenda))
  )

(use-package with-editor
  :ensure t
  :defer t
  :bind (([remap async-shell-command] . with-editor-async-shell-command)
         ([remap shell-command] . with-editor-shell-command)))

(use-package eglot
  :defer t
  ;; :hook (;; (clojure-mode . eglot-ensure)
  ;;        ;; (clojure-ts-mode . eglot-ensure)
  ;;        (go-mode . eglot-ensure)
  ;;        (go-ts-mode . eglot-ensure)
  ;;        )
  :custom ((eglot-autoshutdown t)
           (eglot-connect-timeout 600)
           (eglot-events-buffer-size 0)
           (eglot-extend-to-xref t)
           (eglot-report-progress nil))
  :bind (:map eglot-mode-map
              ("C-c e" . eglot-code-actions))
  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  (setq jsonrpc-event-hook nil)

  (defun xref-find-references-with-eglot (orig-fun &rest args)
    "An advice function that gives xref-find-definitions a unique
buffer name when eglot is enabled."
    (if (bound-and-true-p eglot--managed-mode)
        (let ((xref-buffer-name (format "%s %s"
                                        xref-buffer-name
                                        (symbol-at-point))))
          (apply orig-fun args))
      (apply orig-fun args)))

  (advice-add 'xref-find-references :around
              #'xref-find-references-with-eglot)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun eglot-disable-in-cider ()
    (when (eglot-managed-p)
      (if (bound-and-true-p cider-mode)
          (progn
            (remove-hook 'completion-at-point-functions 'eglot-completion-at-point)
            (remove-hook 'xref-backend-functions 'eglot-xref-backend))
        (add-hook 'completion-at-point-functions 'eglot-completion-at-point nil t)
        (add-hook 'xref-backend-functions 'eglot-xref-backend))))
  (add-hook 'cider-mode-hook #'eglot-disable-in-cider)
  (add-hook 'eglot-managed-mode-hook #'eglot-disable-in-cider))

(use-package jarchive
  :ensure t
  :defer t
  :hook ((clojure-mode . jarchive-mode)
         (clojure-ts-mode . jarchive-mode)))

(use-package jdecomp
  :ensure t
  :defer t
  :custom ((jdecomp-decompiler-paths '((cfr . "~/bin/cfr-0.152.jar"))))
  :config
  (jdecomp-mode 1))

(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package eat
  :ensure t
  :defer t
  :after eshell
  :hook ((eshell-first-time-mode . eat-eshell-visual-command-mode)
         (eshell-first-time-mode . eat-eshell-mode)))

;;; Language major modes

(use-package clojure-mode
  :ensure t
  :defer t
  :custom ((clojure-toplevel-inside-comment-form t))
  :config
  (defun clojure-copy-ns ()
    "Save the current clojure ns to the kill ring."
    (interactive)
    (let ((ns (funcall clojure-expected-ns-function)))
      (-kill-and-echo ns)))
  (defun clojure-copy-ns-var ()
    "Save the current clojure var to the kill ring."
    (interactive)
    (-kill-and-echo
     (format "%s/%s" (clojure-find-ns) (clojure-current-defun-name))))
  (keymap-set clojure-mode-map "C-c w" #'clojure-copy-ns-var)
  (keymap-set clojure-mode-map "C-c W" #'clojure-copy-ns)

  (with-eval-after-load 'project
    (defun project-find-clojure-project (dir)
      (when-let ((root (or (locate-dominating-file dir "project.clj")
                           (locate-dominating-file dir "deps.edn"))))
        (cons 'clojure-project root)))

    (cl-defmethod project-root ((project (head clojure-project)))
      (cdr project))

    ;; (add-hook 'project-find-functions #'project-find-clojure-project)
    ;; (remove-hook 'project-find-functions #'project-find-clojure-project)
    ))

(use-package cider
  :ensure t
  :defer t
  :custom ((cider-eldoc-display-for-symbol-at-point nil)
           (cider-repl-display-help-banner nil)
           (cider-repl-display-in-current-window nil)
           (cider-repl-pop-to-buffer-on-connect 'display-only)
           (cider-test-fail-fast nil)
           (cider-xref-fn-depth 90))
  :bind (:map cider-mode-map
              ("C-c M-." . cider-find-var)
              ("C-c C-j C-j" . cider-eval-print-last-sexp)))

(use-package markdown-mode
  :ensure t
  :defer t
  :hook ((markdown-mode . auto-fill-mode)
         (markdown-mode . variable-pitch-mode)))

(use-package groovy-mode
  :ensure t
  :defer t
  :custom ((groovy-indent-offset 2)))

;; Go lang
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(use-package go-mode
  :ensure t
  :defer t
  :bind (:map go-mode-map
              ("C-c m r" . go-run))
  :config
  (with-eval-after-load 'project
    (defun project-find-go-module (dir)
      (when-let ((root (locate-dominating-file dir "go.mod")))
        (cons 'go-module root)))

    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))

    (add-hook 'project-find-functions #'project-find-go-module))

  (defun before-save-go-mode ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (add-hook 'go-mode-hook #'before-save-go-mode)
  (add-hook 'go-ts-mode-hook #'before-save-go-mode))

(use-package gotest
  :ensure t
  :defer t
  :after go-mode
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)
              ("C-c t j" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t x" . go-run)))

(use-package jinja2-mode
  :ensure t
  :defer t)

(use-package dape
  :ensure t
  :defer t
  :hook (dape-display-source . pulse-momentary-highlight-one-line)
  :config

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)

  ;; Add Go debug configuration
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-request-timeout 600)
  (setq dape-inlay-hints t)
  (dape-breakpoint-global-mode)
  (add-to-list 'dape-configs
               `(go-test
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport" "--log")
                 command-cwd default-directory ;dape-command-cwd
                 ;; host "127.0.0.1"
                 port :autoport
                 :type "go"
                 :name "debug test"
                 :request "launch"
                 :mode "test"
                 ;; :cwd default-directory
                 :showLog "true"
                 :program "."
                 :buildFlags ["-tags=integration"]
                 :args ["-ginkgo.failFast" "-ginkgo.trace" "-ginkgo.noColor" "-ginkgo.focus" "trains on all documents and inserts all the training metadata when it's done"])))

(use-package corfu
  :ensure t
  :defer t
  :hook ((after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator))
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer."
    (when (local-variable-p 'completion-at-point-functions)
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; (defun corfu-move-to-minibuffer ()
  ;;   (interactive)
  ;;   (pcase completion-in-region--data
  ;;     (`(,beg ,end ,table ,pred ,extras)
  ;;      (let ((completion-extra-properties extras)
  ;;            completion-cycle-threshold completion-cycling)
  ;;        (completing-read-at-point beg end table pred)))))
  ;; (keymap-unset corfu-map "M-m")
  ;; (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  )

(use-package sly
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (defun sly-eval-last-expression-in-repl ()
    "Evaluates last expression in the Sly mREPL."
    (interactive)
    (let ((expr (sly-last-expression))
          (buffer-name (buffer-name (current-buffer)))
          (new-package (sly-current-package))
          (yank-back nil))
      (with-current-buffer (sly-mrepl--find-buffer)
        (unless (eq (current-buffer) (window-buffer))
          (pop-to-buffer (current-buffer) t))
        ;; Kill pending input in the REPL
        (when (< (marker-position (sly-mrepl--mark)) (point))
          (let ((inhibit-read-only t))
            (kill-region (marker-position (sly-mrepl--mark)) (point)))
          (setq yank-back t))
        (goto-char (point-max))
        (insert-before-markers (format "\n;;; from %s\n" buffer-name))
        (when new-package
          (sly-mrepl-set-package new-package))
        (insert expr)
        (sly-mrepl-return)
        ;; Put pending input back
        (when yank-back
          (yank)))))
  (keymap-set sly-mode-map "C-c C-j" 'sly-eval-last-expression-in-repl))

(use-package sqlformat
  :ensure t
  :defer t
  :custom ((sqlformat-args '("-s2" "-g"))
           (sqlformat-command 'pgformatter))
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat-buffer)))

(use-package jinx
  :ensure t
  :defer t
  :hook (after-init . global-jinx-mode)
  :bind (("s-;" . jinx-correct))
  :custom ((jinx-languages "en_US en_CA")))

(use-package cape
  :ensure t
  :defer t
  :bind (("C-c p" . cape-prefix-map))
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package pulsar
  :ensure t
  :defer t
  :custom ((pulsar-face 'pulsar-blue))
  :config
  (pulsar-global-mode))

(use-package casual-dired
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)))

(unless (package-installed-p 'eglot-booster)
  (package-vc-install "https://github.com/jdtsmith/eglot-booster"))

(use-package eglot-booster
  :ensure t
  :defer t
  :after eglot
  :config
  (eglot-booster-mode))

;;; Pairing

(use-package keycast
  :ensure t
  :defer t)

(define-minor-mode pair-mode
  "Set up Emacs for pairing"
  :global t
  :lighter " 🍐"
  (cond
   (pair-mode
    (global-display-line-numbers-mode 1)
    (keycast-header-line-mode 1)
    (global-text-scale-adjust 2))
   (t
    (global-display-line-numbers-mode -1)
    (keycast-header-line-mode -1)
    (global-text-scale-adjust -2))))

(keymap-global-set "<f8>" #'pair-mode)

;;; Just chillin

(define-minor-mode chill-mode
  "Set up Emacs for focusing"
  :global t
  :lighter " Chilling"
  (cond
   (chill-mode
    (setq line-spacing 0.1)
    ;; (setq spacious-padding-subtle-mode-line `(:mode-line-active 'default :mode-line-inactive vertical-border))
    (setq spacious-padding-subtle-mode-line nil)
    ;; (font-lock-mode -1)
    ;; (let ((fullscreen (frame-parameter nil 'fullscreen)))
    ;;   (unless (memq fullscreen '(fullscreen fullboth))
    ;;     (toggle-frame-fullscreen)))
    (spacious-padding-mode 1))
   (t
    (setq line-spacing nil)
    (setq spacious-padding-subtle-mode-line nil)
    ;; (font-lock-mode 1)
    ;; (let ((fullscreen (frame-parameter nil 'fullscreen)))
    ;;   (when (memq fullscreen '(fullscreen fullboth))
    ;;     (toggle-frame-fullscreen)))
    (spacious-padding-mode -1))))

(keymap-global-set "<f7>" #'chill-mode)


(provide 'init)

;;; init.el ends here
