;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;; This config targets Emacs 30

;;; Code:

;;; Package Management

(defmacro external-package (package &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent 1) (debug (form def-body)))
  `(if (package-installed-p ',package)
       (progn ,@body)
     (warn "External package \"%s\" is not installed... skipping config."
           (symbol-name ',package))))

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
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (zig "https://github.com/GrayJack/tree-sitter-zig")))

(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
        (clojurec-mode . clojure-ts-mode)
        (clojurescript-mode . clojure-ts-mode)
        (clojure-mode . clojure-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (go-dot-mod-mode . go-mod-ts-mode)
        (mhtml-mode . html-ts-mode)
        (sgml-mode . html-ts-mode)
        (java-mode . java-ts-mode)
        (js-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(defun install-latest-known-treesitter-grammars ()
  "Install/upgrade all latest Tree-sitter grammars."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (message "Downloading %s treesitter grammar from %s" (car grammar) (cadr grammar))
    (treesit-install-language-grammar (car grammar))))

;;; Settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-list-file-prefix (expand-file-name "var/auto-save/" user-emacs-directory))
 '(backup-by-copying t)
 '(backup-directory-alist
   (list
    (cons "."
          (expand-file-name "var/backup/" user-emacs-directory))))
 '(calendar-mark-holidays-flag t)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-display-in-current-window nil)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(cider-xref-fn-depth 90)
 '(clojure-toplevel-inside-comment-form t)
 '(column-number-mode t)
 '(completion-category-overrides '((file (styles basic orderless))))
 '(completion-styles '(orderless))
 '(context-menu-mode t)
 '(custom-safe-themes
   '("14436a10b0cb5b7b6e6f6d490a08c1a751ad0384e9b124b9b8d5d554129f5571" default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(echo-keystrokes 0.2)
 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(eglot-connect-timeout 600)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(erc-auto-query 'bury)
 '(erc-fill-function 'erc-fill-static)
 '(erc-fill-static-center 20)
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-server "irc.libera.chat" t)
 '(fill-column 80)
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(global-goto-address-mode t)
 '(global-so-long-mode t)
 '(grep-find-command '("rg -n -H --no-heading --glob='' -e ''" . 37))
 '(helpful-switch-buffer-function #'helpful-switch-to-buffer)
 '(history-length 20000)
 '(hscroll-margin 2)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(isearch-allow-motion t)
 '(isearch-allow-scroll t)
 '(isearch-lazy-count t)
 '(isearch-wrap-pause 'no)
 '(isearch-yank-on-move t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(kill-do-not-save-duplicates t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format " [%s/%s]")
 '(line-spacing 3)
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 '(magit-log-margin '(t age-abbreviated magit-log-margin-width t 15))
 '(make-backup-files t)
 '(mode-line-compact 'long)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-scroll-amount
   '(2
     ((shift)
      . hscroll)
     ((meta))
     ((control meta)
      . global-text-scale)
     ((control)
      . text-scale)))
 '(mouse-wheel-scroll-amount-horizontal 2)
 '(mouse-wheel-tilt-scroll t)
 '(next-error-message-highlight t)
 '(ns-use-proxy-icon nil t)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/org/journal/journal.org"))
 '(org-agenda-span 'day)
 '(org-agenda-start-with-log-mode t)
 '(org-agenda-time-grid
   '((daily today require-timed)
     (600 1600)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
 '(org-agenda-window-setup 'current-window)
 '(org-attach-auto-tag "attachment")
 '(org-babel-load-languages '((emacs-lisp . t) (sql . t)))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+olp+datetree "~/org/journal/journal.org")
      "* TODO %?\12SCHEDULED: %t\12" :clock-in t :clock-resume t :tree-type week)
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
 '(org-clock-clocked-in-display nil)
 '(org-clock-clocktable-default-properties '(:maxlevel 4))
 '(org-cycle-separator-lines 2)
 '(org-default-notes-file "~/org/journal/inbox.org")
 '(org-directory "~/org")
 '(org-edit-src-content-indentation 0)
 '(org-ellipsis " ¶")
 '(org-fontify-done-headline nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-block-startup nil)
 '(org-image-actual-width 640)
 '(org-indirect-buffer-display 'current-window)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-refile-use-outline-path 'file)
 '(org-special-ctrl-a/e 'reversed)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'current-window)
 '(org-startup-folded 'content)
 '(org-tags-column -80)
 '(org-todo-keyword-faces
   '(("NEXT" . "blue")
     ("REVIEW" . "dark orange")
     ("HOLD" . "purple")
     ("CANCELLED" . "teal")))
 '(org-todo-keywords
   '((sequence "TODO(t@/)" "NEXT(n)" "REVIEW(r@/!)" "|" "DONE(d!)")
     (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-use-speed-commands t)
 '(package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(racket-mode groovy-mode dumb-jump clojure-ts-mode csv-mode which-key markdown-toc zig-mode lua-mode fennel-mode olivetti avy cider clojure-mode eglot embark emmet-mode flyspell go-mode helpful iedit magit markdown-mode orderless org rainbow-mode rg sly sqlformat vertico ws-butler yaml-mode))
 '(pixel-scroll-precision-mode t)
 '(recentf-max-saved-items 200)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'flash-mode-line)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring))
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position t)
 '(search-whitespace-regexp ".*?")
 '(show-paren-context-when-offscreen t)
 '(show-paren-mode t)
 '(shr-cookie-policy nil)
 '(shr-discard-aria-hidden t)
 '(shr-image-animate nil)
 '(shr-indentation 0 t)
 '(shr-max-image-proportion 0.5)
 '(shr-use-colors nil)
 '(shr-use-fonts nil)
 '(shr-width 72)
 '(sqlformat-args '("-s2" "-g"))
 '(sqlformat-command 'pgformatter)
 '(tab-always-indent 'complete)
 '(tab-width 8)
 '(tramp-default-method "ssh")
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(version-control t)
 '(visible-bell nil)
 '(word-wrap t)
 '(ws-butler-keep-whitespace-before-point nil)
 '(x-stretch-cursor t)
 '(xref-search-program 'ripgrep))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))
 '(fixed-pitch ((t (:family "Iosevka" :height 150))))
 '(variable-pitch ((t (:family "Sans Serif" :height 170)))))

(setq-default
 frame-title-format '("%n "              ; narrowed?
                      (:eval
                       (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b")))
 mode-line-format
 (remove '(vc-mode vc-mode) mode-line-format))

(defun flash-mode-line ()
  "Flash the modeline on error or warning.
https://macowners.club/posts/custom-functions-4-ui/"
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;; Keybindings

(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x k" #'kill-this-buffer)
(keymap-global-set "C-M-r" #'raise-sexp)
(keymap-global-set "C-M-d" #'down-list)
(keymap-global-set "C-M-w" #'backward-kill-sexp)
(keymap-global-set "C-M-<return>" #'newline-at-end-of-line)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-i" #'delete-other-windows)
(keymap-global-set "M-SPC" #'cycle-spacing)
(keymap-global-set "M-Z" #'zap-to-char)
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "C-c d" #'find-config)
(keymap-global-set "C-c t t" #'load-one-theme)
(keymap-global-set "C-c t w" #'whitespace-mode)
(keymap-global-set "C-c t m" #'toggle-frame-maximized)
(keymap-global-set "C-c t M" #'toggle-frame-fullscreen)
(keymap-global-set "C-c t $" #'toggle-truncate-lines)
(keymap-global-set "C-\\" #'undo-only)
(keymap-global-set "C-h p" #'describe-package)  ; Swap the two
(keymap-global-set "C-h P" #'finder-by-keyword)
(keymap-global-set "C-h L" #'find-library)
(keymap-global-set "C-z" nil)
(keymap-substitute global-map #'eval-expression #'pp-eval-expression) ; M-:
(keymap-substitute global-map #'eval-last-sexp #'pp-eval-last-sexp)   ; C-x C-e
(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line+) ; C-a
(keymap-global-set "C-c r" #'recentf-open-files+)
(keymap-global-set "C-w" #'backward-kill-word-or-region)
(global-set-key (kbd "M-q") #'fill-or-unfill) ; M-q
(keymap-set window-prefix-map "S" #'window-toggle-side-windows) ; Was "s" but rarely used
(keymap-set window-prefix-map "s" #'toggle-window-split)
(keymap-set window-prefix-map "t" #'window-swap-states) ; aka transpose windows

;;; Functions

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

(defun completing-read-at-point (start end col &optional pred)
  "Inspired by https://github.com/katspaugh/ido-at-point"
  (if (minibufferp) (completion--in-region start end col pred)
    (let* ((init (buffer-substring-no-properties start end))
           (all (completion-all-completions init col pred (length init)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read "Completions: " col pred t init)))))
      (if completion
          (progn
            (delete-region start end)
            (insert completion)
            t)
        (message "No completions") nil))))

(setq completion-in-region-function #'completing-read-at-point)

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

;;; Mac

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'meta       ; Backup meta key
        trash-directory "~/.Trash"
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aFGhlv --group-directories-first"))

;;; Theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes/sketch-themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/sketch-themes/")

(add-hook 'after-init-hook
          (lambda () (load-theme 'alabaster t)))

;;; Built-in Packages

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-post-open-functions
            (lambda (&rest _)
              (call-interactively #'tab-bar-rename-tab))))

(ffap-bindings)

(add-hook 'text-mode-hook 'visual-line-mode)

(with-eval-after-load 'dired
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (keymap-set global-map "C-x C-j" #'dired-jump)
  (require 'dired-x)
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(with-eval-after-load 'project
  ;; Setup the `project-switch-commands'
  (require 'magit-extras)

  (defun project-find-regexp-with-unique-buffer (orig-fun &rest args)
    "An advice function that gives project-find-regexp a unique buffer name"
    (require 'xref)
    (let ((xref-buffer-name (format "%s %s" xref-buffer-name (car args))))
      (apply orig-fun args)))

  (advice-add 'project-find-regexp :around
              #'project-find-regexp-with-unique-buffer))

(with-eval-after-load 'compile
  (require 'ansi-color)
  (ansi-color-for-comint-mode-filter))

(add-to-list 'auto-mode-alist
             '("\\.log\\'" . auto-revert-tail-mode))

(with-eval-after-load 'isearch
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

  (keymap-set isearch-mode-map "C-o" #'isearch-occur)
  (keymap-set isearch-mode-map "C-<backspace>" #'isearch-delete-wrong)
  ;; DEL during isearch should edit the search string, not jump back to the
  ;; previous result
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char)

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(with-eval-after-load 'rect
  ;; https://gist.github.com/jdtsmith/bfa2d692c4fbbffe06b558e4bcf9abec
  (cl-loop for (key def) in
	   '(("k" kill-rectangle)   	("t" string-rectangle)
	     ("o" open-rectangle)   	("w" copy-rectangle-as-kill)
	     ("y" yank-rectangle)   	("c" clear-rectangle)
	     ("d" delete-rectangle) 	("N" rectangle-number-lines)
	     (" " delete-whitespace-rectangle)
	     ("=" calc-grab-sum-across) ("+" calc-grab-sum-down)
	     ("#" calc-grab-rectangle)  ("n" set-mark-command)
	     ("q" (lambda () (interactive) (deactivate-mark)))
	     ("?" (lambda () (interactive)
                    (embark-bindings-in-keymap rectangle-mark-mode-map))))
	   do (define-key rectangle-mark-mode-map key def)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

;;; 3rd Party Packages

(external-package orderless
  (setq completion-category-defaults nil))

(external-package vertico
  (vertico-mode)

  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index start)
                (ignore start)
                (setq cand (funcall orig cand prefix suffix index start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

;; https://github.com/minad/cape
(defun capf-complete-filename ()
  "Complete filename at point"
  (let* ((bounds (or (bounds-of-thing-at-point 'filename)
                     (cons (point) (point))))
         (file (buffer-substring (car bounds) (cdr bounds))))
    (when (and (string-match-p "/" file)
               (file-exists-p (file-name-directory file)))
      `(,(car bounds) ,(cdr bounds) ,#'read-file-name-internal
        ,@(and (not (equal file "/")) (string-suffix-p "/" file)
               '(:company-prefix-length t))
        :exclusive no
        :annotation-function (lambda (s) (if (string-suffix-p "/" s) " Folder" " File"))
        :company-kind (lambda (s) (if (string-suffix-p "/" s) 'folder 'file))))))

(add-to-list 'completion-at-point-functions #'capf-complete-filename)

(external-package embark
  (keymap-global-set "C-." #'embark-act)
  (keymap-global-set "C-h B" #'embark-bindings)
  (keymap-global-set "M-n" #'embark-next-symbol)
  (keymap-global-set "M-s n" #'embark-next-symbol)
  (keymap-global-set "M-p" #'embark-previous-symbol)
  (keymap-global-set "M-s p" #'embark-previous-symbol)
  (autoload #'embark-next-symbol "embark" nil t)
  (autoload #'embark-previous-symbol "embark" nil t)
  (setq prefix-help-command #'embark-prefix-help-command))

(external-package avy
  (keymap-global-set "M-j" 'avy-goto-char-timer))

(external-package helpful
  ;; Remap standard commands.
  (keymap-substitute global-map #'describe-function #'helpful-callable)
  (keymap-substitute global-map #'describe-variable #'helpful-variable)
  (keymap-substitute global-map #'describe-key      #'helpful-key)
  (keymap-substitute global-map #'describe-symbol   #'helpful-symbol)
  (keymap-global-set "C-c C-d" #'helpful-at-point)
  (keymap-global-set "C-h C"   #'helpful-command)
  (keymap-global-set "C-h F"   #'describe-face)

  ;; https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun helpful-switch-to-buffer (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))))

(external-package ws-butler
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (custom-set-variables
   '(ws-butler-keep-whitespace-before-point nil)))

(external-package iedit
  (keymap-global-set "C-;" 'iedit-mode))

;; Org mode

(defun org-mode-setup ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))

(add-hook 'org-mode-hook 'org-mode-setup)
(add-hook 'org-mode-hook 'auto-fill-mode)

(autoload #'org-store-link "org" nil t)
(autoload #'org-agenda "org" nil t)
(autoload #'org-switchb "org" nil t)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c b" #'org-switchb)
(keymap-global-set "C-c g" #'org-clock-goto)
(global-set-key (kbd "C-c c") #'org-capture)

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," nil)

  (require 'ox-md)
  (require 'org-tempo)
  (require 'org-habit)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(external-package with-editor
  (keymap-substitute global-map #'async-shell-command #'with-editor-async-shell-command)
  (keymap-substitute global-map #'shell-command #'with-editor-shell-command))

(external-package eglot
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  (add-hook 'clojure-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-mode 'eglot-ensure)
  (add-hook 'go-ts-mode 'eglot-ensure)

  (with-eval-after-load 'eglot
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
                #'xref-find-references-with-eglot)))

(external-package dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (with-eval-after-load 'xref
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

;;; Language major modes

(external-package clojure-mode
  (with-eval-after-load 'clojure-mode
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
    (keymap-set clojure-mode-map "C-c W" #'clojure-copy-ns)))

(external-package clojure-ts-mode
  (with-eval-after-load 'clojure-ts-mode
    (require 'clojure-mode)
    (setq clojure-ts-mode-syntax-table clojure-mode-syntax-table)

    (keymap-set clojure-ts-mode-map "C-c M-x" #'cider)
    (keymap-set clojure-ts-mode-map "C-c M-j" #'cider-jack-in-clj)
    (keymap-set clojure-ts-mode-map "C-c M-J" #'cider-jack-in-cljs)
    (keymap-set clojure-ts-mode-map "C-c M-c" #'cider-connect-clj)
    (keymap-set clojure-ts-mode-map "C-c M-C" #'cider-connect-cljs)
    (keymap-set clojure-ts-mode-map "C-c C-x" 'cider-start-map)
    (keymap-set clojure-ts-mode-map "C-c C-s" 'sesman-map)
    (require 'sesman)
    (sesman-install-menu clojure-ts-mode-map)
    (add-hook 'clojure-ts-mode-hook
              (lambda ()
                (setq-local sesman-system 'CIDER)

                ;; Fix C-M-t transpose-sexps b/c today treesit-transpose-sexps
                ;; doesn't do the job.
                (setq-local transpose-sexps-function
                            #'transpose-sexps-default-function)))
    (add-hook 'clojure-ts-mode-hook #'clojure-mode-variables)

    (defun cider-repl-type-for-buffer-in-clojure-ts-mode (&optional buffer)
      "Determine repl type for clojure-ts-mode buffers."
      (with-current-buffer (or buffer (current-buffer))
        (when (and buffer-file-name (derived-mode-p 'clojure-ts-mode))
          (pcase (file-name-extension buffer-file-name)
            ("cljs" 'cljs)
            ("cljc" 'multi)
            ("clj" 'clj)))))

    (advice-add #'cider-repl-type-for-buffer
                ;; Fallback to the advice when cider fails to find it
                :after-until
                #'cider-repl-type-for-buffer-in-clojure-ts-mode)

    (keymap-set clojure-ts-mode-map "C-c w" #'clojure-copy-ns-var)
    (keymap-set clojure-ts-mode-map "C-c W" #'clojure-copy-ns))

  (add-hook 'clojure-ts-mode-hook #'cider-mode)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((clojure-mode
                                           clojurescript-mode
                                           clojurec-mode
                                           clojure-ts-mode)
                                          . ("clojure-lsp")))))

(external-package cider
  (with-eval-after-load 'cider
    (keymap-set cider-mode-map "C-c M-." 'cider-find-var)))

(external-package markdown-mode
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (setq markdown-command "marked"))

(external-package go-mode
  (with-eval-after-load 'project
    (defun project-find-go-module (dir)
      (when-let ((root (locate-dominating-file dir "go.mod")))
        (cons 'go-module root)))

    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))

    (add-hook 'project-find-functions #'project-find-go-module))

  (add-hook 'before-save-hook #'gofmt-before-save))

(external-package flyspell
  (with-eval-after-load 'flyspell
    ;; (keymap-set flyspell-mode-map "C-M-i" nil) ; Reserved for completion-at-point
    ;; (keymap-set flyspell-mode-map "C-." nil)  ; Reserved for embark-act
    ;; (keymap-set flyspell-mode-map "C-;" nil) ; Reserved for iedit
    ))

(external-package emmet-mode
  (add-hook 'html-mode 'emmet-mode)
  (add-hook 'css-mode 'emmet-mode))

(external-package sly
  (setq inferior-lisp-program "sbcl")
  (with-eval-after-load 'sly
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
    (keymap-set sly-mode-map "C-c C-j" 'sly-eval-last-expression-in-repl)))

(external-package sqlformat
  (with-eval-after-load 'sql
    (keymap-set sql-mode-map "C-c C-f" #'sqlformat-buffer)))

(define-minor-mode cz-mode
  "Toggle my personal C-z mode for navigation."
  :init-value nil
  :global t
  :lighter " 🥨"
  :group 'cz
  :keymap
  '(("n" . next-line)
    ("p" . previous-line)
    ("f" . forward-char)
    ("b" . backward-char)
    ("q" . cz-mode)                     ; Quit
    ([up] . scroll-down)
    ([down] . scroll-up)
    ;; A pinch of evil for one-handed navigation
    ("h" . backward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("l" . forward-char)))

(keymap-global-set "C-z" #'cz-mode)

(provide 'init)

;;; init.el ends here
