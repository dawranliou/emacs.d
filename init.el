;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;; This config targets Emacs 29

;;; Code:

;;; Package Management

(defmacro with-eval-after-package-install (package &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent defun))
  `(if (package-installed-p ,package)
       (progn ,@body)
     (message (concat "Package \'"
                      (symbol-name ,package)
                      "\' is not installed... skipping config."))))

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
 '(cider-repl-buffer-size-limit 100000)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-display-in-current-window nil)
 '(cider-repl-pop-to-buffer-on-connect 'display-only)
 '(cider-repl-use-pretty-printing t)
 '(cider-xref-fn-depth 90)
 '(clojure-toplevel-inside-comment-form t)
 '(column-number-mode t)
 '(completion-category-overrides '((file (styles basic orderless))))
 '(completion-styles '(orderless))
 '(context-menu-mode t)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
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
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(global-so-long-mode t)
 '(grep-find-command '("rg -n -H --no-heading --glob='' -e ''" . 37))
 '(helpful-switch-buffer-function #'helpful-switch-to-buffer)
 '(history-length 20000)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(isearch-allow-motion t)
 '(isearch-allow-scroll t)
 '(isearch-lazy-count t)
 '(isearch-wrap-pause 'no)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format " [%s/%s]")
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 '(magit-log-margin '(t age-abbreviated magit-log-margin-width t 15))
 '(make-backup-files t)
 '(mode-line-compact 'long)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-tilt-scroll t)
 '(next-error-message-highlight t)
 '(ns-use-proxy-icon nil t)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/org/journal/journal.org"))
 '(org-agenda-span 'day)
 '(org-agenda-start-with-log-mode t)
 '(org-agenda-window-setup 'current-window)
 '(org-attach-auto-tag "attachment")
 '(org-babel-load-languages '((emacs-lisp . t) (sql . t)))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file+olp+datetree "~/org/journal/journal.org")
      "* TODO %?\12" :clock-in t :clock-resume t :tree-type week)
     ("j" "Journal" entry
      (file+olp+datetree "~/org/journal/journal.org")
      "* %? %^G\12" :clock-in t :clock-keep t :tree-type week)
     ("d" "Daily Review" entry
      (file+olp+datetree "~/org/journal/journal.org")
      (file "~/.emacs.d/org-templates/daily-review.org")
      :immediate-finish t :clock-in t :clock-keep t :tree-type week)
     ("i" "Check In" entry
      (file+olp+datetree "~/org/journal/journal.org")
      "* Check in %T  :process:\12#+BEGIN: clocktable :maxlevel 4 :tcolumns 1 :block %<%Y-%m-%d> \12#+END:" :immediate-finish t :tree-type week)
     ("m" "Meeting" entry
      (file+olp+datetree "~/org/journal/journal.org")
      "* %^{Meeting} :meeting:%^G\12" :immediate-finish t :clock-in t :clock-keep t :tree-type week)))
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
 '(org-roam-directory "~/org/roam/")
 '(org-special-ctrl-a/e 'reversed)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation nil)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'current-window)
 '(org-startup-folded 'content)
 '(org-tags-column -80)
 '(org-todo-keyword-faces
   '(("NEXT" . "blue")
     ("WAITING" . "dark orange")
     ("HOLD" . "purple")
     ("CANCELLED" . "teal")))
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-use-speed-commands t)
 '(package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(markdown-toc zig-mode lua-mode fennel-mode olivetti avy cider clojure-mode eglot embark emmet-mode flyspell go-mode helpful iedit magit markdown-mode orderless org org-roam rainbow-mode rg sly sqlformat vertico ws-butler yaml-mode))
 '(pixel-scroll-precision-mode t)
 '(recentf-max-saved-items 200)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(save-place-mode t)
 '(savehist-additional-variables
   '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring))
 '(savehist-mode t)
 '(savehist-save-minibuffer-history t)
 '(scroll-conservatively 101)
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
 '(tramp-default-method "ssh")
 '(version-control t)
 '(visible-bell nil)
 '(winner-mode t)
 '(ws-butler-keep-whitespace-before-point nil)
 '(xref-search-program 'ripgrep))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))
 '(fixed-pitch ((t (:family "Iosevka" :height 140))))
 '(variable-pitch ((t (:family "Sans Serif" :height 170)))))

(setq-default
 fill-column 80
 x-stretch-cursor t
 tab-width 8
 tab-always-indent t
 line-spacing 3
 indent-tabs-mode nil
 truncate-lines t)

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
(keymap-global-set "C-x C-r" #'recentf-open-files+)
(keymap-global-set "C-w" #'backward-kill-word-or-region)
(keymap-global-set "M-Q" #'unfill-paragraph)
(global-set-key (kbd "M-q") #'fill-or-unfill) ; M-q
(keymap-set ctl-x-4-map "s" #'toggle-window-split)
(keymap-set ctl-x-4-map "t" #'transpose-windows)

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

(defun load-one-theme-action (theme)
  "Disable current themes and load theme THEME."
  (progn
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)))

(defun load-one-theme ()
  "Disable current themes and load theme from the completion list."
  (interactive)
  (let ((theme (completing-read "Load custom theme: "
                                (mapcar 'symbol-name
                                        (custom-available-themes)))))
    (load-one-theme-action theme)))

(defun set-font ()
  "Select xfont."
  (interactive)
  (set-frame-font (completing-read
                   "Choose font:"
                   (cl-remove-duplicates (x-list-fonts "*") :test #'equal))))

(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (unless recentf-mode
    (recentf-mode +1))
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
  "Generate a new UUID and insert."
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))

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
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows.")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
          (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((this-buffer (window-buffer))
        (this-window (selected-window)))
    (other-window arg)
    (set-window-buffer this-window (current-buffer))
    (set-window-buffer (selected-window) this-buffer)))

(defun fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

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

;; (setq-default
;;  mode-line-format
;;  `(""
;;    mode-line-front-space
;;    mode-line-mule-info
;;    mode-line-client
;;    mode-line-modified
;;    mode-line-remote
;;    ;; " " mode-line-buffer-identification
;;    (:propertize " %b" face mode-line-buffer-id)
;;    " %l:%c (%p)"
;;    " ["
;;    (:propertize ("" mode-name)
;;                 help-echo "Major mode\n\
;; mouse-1: Display major mode menu\n\
;; mouse-2: Show help for major mode\n\
;; mouse-3: Toggle minor modes"
;;                 mouse-face mode-line-highlight
;;                 local-map ,mode-line-major-mode-keymap)
;;    "]"
;;    mode-line-misc-info))

(add-hook 'after-init-hook
          (lambda () (load-theme 'sketch-white t)))

;;; Built-in Packages

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-post-open-functions
            (lambda (&rest _)
              (call-interactively #'tab-bar-rename-tab))))

(ffap-bindings)

(with-eval-after-load 'dired
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (keymap-set global-map "C-x C-j" #'dired-jump)
  (require 'dired-x)
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(defun eshell-history ()
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

(with-eval-after-load 'esh-mode
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; Use Ivy to provide completions in eshell
  (keymap-set eshell-mode-map "C-r" #'eshell-history)
  (keymap-set eshell-mode-map "C-a" #'eshell-bol)
  (setq eshell-history-size          10000
        eshell-buffer-maximum-lines  10000
        eshell-hist-ignoredups           t
        eshell-highlight-prompt          t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t))

(with-eval-after-load 'project
  ;; Setup the `project-switch-commands'
  (require 'magit-extras))

(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

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

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))

;;; 3rd Party Packages

(with-eval-after-package-install 'orderless
  (setq completion-category-defaults nil))

(with-eval-after-package-install 'vertico
  (vertico-mode)

  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
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

(defun completion-at-point-filename ()
  "Interactive command to complete filename at point"
  (interactive)
  (let ((completion-at-point-functions '(capf-complete-filename)))
    (completion-at-point)))

(keymap-global-set "C-c C-f" #'completion-at-point-filename)

(with-eval-after-package-install 'embark
  (keymap-global-set "C-." #'embark-act)
  (keymap-global-set "C-h B" #'embark-bindings)
  (keymap-global-set "M-n" #'embark-next-symbol)
  (keymap-global-set "M-s n" #'embark-next-symbol)
  (keymap-global-set "M-p" #'embark-previous-symbol)
  (keymap-global-set "M-s p" #'embark-previous-symbol)
  (autoload #'embark-next-symbol "embark" nil t)
  (autoload #'embark-previous-symbol "embark" nil t)
  (setq prefix-help-command #'embark-prefix-help-command))

(with-eval-after-package-install 'avy
  (keymap-global-set "M-j" 'avy-goto-char-timer))

(with-eval-after-package-install 'helpful
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

(with-eval-after-package-install 'ws-butler
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode-hook 'ws-butler-mode)
  (custom-set-variables
   '(ws-butler-keep-whitespace-before-point nil)))

(with-eval-after-package-install 'iedit
  (keymap-global-set "C-;" 'iedit-mode))

;; Org mode

(defun org-mode-setup ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c)
                 (if (char-equal c ?<)
                     t
                   (,electric-pair-inhibit-predicate c)))))

(add-hook 'org-mode-hook 'org-mode-setup)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

(autoload #'org-store-link "org" nil t)
(autoload #'org-agenda "org" nil t)
(autoload #'org-switchb "org" nil t)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c b" #'org-switchb)
(global-set-key (kbd "C-c c") #'org-capture)



(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," nil)

  (require 'ox-md)
  (require 'org-tempo)
  (require 'org-habit)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(with-eval-after-package-install 'org-roam
  (setq org-roam-v2-ack t)
  (autoload #'org-roam-node-find "org-roam" nil t)
  (autoload #'org-roam-capture "org-roam" nil t)
  (autoload #'org-roam-node-insert "org-roam" nil t)
  (keymap-global-set "C-c n f" #'org-roam-node-find)
  (keymap-global-set "C-c n i" #'org-roam-node-insert)
  (keymap-global-set "C-c n c" #'org-roam-capture)

  (with-eval-after-load 'org-roam
    (org-roam-setup)
    (keymap-global-set "C-c n g" #'org-roam-graph)
    (keymap-global-set "C-c n l" #'org-roam-buffer-toggle)))

(with-eval-after-package-install 'with-editor
  (keymap-substitute global-map #'async-shell-command #'with-editor-async-shell-command)
  (keymap-substitute global-map #'shell-command #'with-editor-shell-command))

(with-eval-after-package-install 'rg
  (keymap-global-set "C-c s" #'rg)
  (with-eval-after-load 'rg
    (rg-enable-default-bindings)))

(with-eval-after-package-install 'eglot
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  (add-hook 'go-mode 'eglot-ensure)

  ;; Debug clojure-lsp
  ;; (with-eval-after-load 'eglot
  ;;   (add-to-list 'eglot-server-programs
  ;;                '(clojure-mode "~/projects/clojure-lsp/clojure-lsp")))
  )

;;; Language major modes

(with-eval-after-package-install 'clojure-mode
  (with-eval-after-load 'clojure-mode
    (defun clojure-copy-ns ()
      "Save the current clojure ns to the kill ring."
      (interactive)
      (let ((ns (funcall clojure-expected-ns-function)))
        (-kill-and-echo ns)))
    (defun clojure-copy-ns-var ()
      "Save the current clojure var to the kill ring."
      (interactive)
      (let ((ns (funcall clojure-expected-ns-function))
            (def (clojure-find-def)))
        (-kill-and-echo (format "%s/%s" ns (cadr def)))))
    (keymap-set clojure-mode-map "C-c w" #'clojure-copy-ns-var)
    (keymap-set clojure-mode-map "C-c W" #'clojure-copy-ns)))

(with-eval-after-package-install 'cider
  (with-eval-after-load 'cider
    (add-to-list 'mode-line-misc-info
                 `(cider-mode (" [" (:eval (cider--modeline-info)) "]")))
    (keymap-set cider-mode-map "C-c M-." 'cider-find-var)))

(with-eval-after-package-install 'markdown-mode
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (setq markdown-command "marked"))

(with-eval-after-package-install 'go-mode
  (with-eval-after-load 'go-mode
    (add-hook 'before-save-hook #'gofmt-before-save)))

(with-eval-after-package-install 'flyspell
  (with-eval-after-load 'flyspell
    (keymap-set flyspell-mode-map "C-M-i" nil) ; Reserved for completion-at-point
    (keymap-set flyspell-mode-map "C-." nil)  ; Reserved for embark-act
    (keymap-set flyspell-mode-map "C-;" nil)) ; Reserved for iedit
  (add-hook 'text-mode-hook 'flyspell-mode))

(with-eval-after-package-install 'emmet-mode
  (add-hook 'html-mode 'emmet-mode)
  (add-hook 'css-mode 'emmet-mode))

(with-eval-after-package-install 'sly
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

(with-eval-after-package-install 'sqlformat
  (with-eval-after-load 'sql
    (keymap-set sql-mode-map "C-c C-f" #'sqlformat-buffer)))

(provide 'init)

;;; init.el ends here
