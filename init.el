;;; init.el --- Daw-Ran Liou's emacs configuration -*- lexical-binding: t; -*-

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d

;;; Commentary:

;; This config targets Emacs 29

;;; Code:

;;; Package Management

;; The very first thing I do is setup the packages I need. I do this so that
;; when I open this config on a new machine, all the packages needed to make it
;; work are specified in `package-selected-packages', with that one you can
;; install them with `package-install-selected-packages'. At which point the
;; whole config should be ready to rock.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archive-priorities '(("melpa" . 30)
                                   ("gnu" . 20)
                                   ("nongnu" . 10)))

(setq package-selected-packages
      '(avy
        cider
        clojure-mode
        corfu
        eglot
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
        ;;lsp-mode
        magit
        markdown-mode
        orderless
        org
        org-roam
        rainbow-mode
        rg
        sly
        smartscan
        sqlformat
        vertico
        ws-butler
        yaml-mode))

(package-initialize)

(defmacro with-eval-after-package-install (package &rest body)
  "Eval BODY only if PACKAGE is installed."
  (declare (indent defun))
  `(if (package-installed-p ,package)
       (progn ,@body)
     (message (concat "Package \'"
                      (symbol-name ,package)
                      "\' is not installed... skipping config."))))

;;; Settings

;; Persist the settings in the custom.el, which is ignored by git.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(custom-set-variables
 '(inhibit-startup-message t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(tramp-default-method "ssh")
 '(delete-by-moving-to-trash t)

 ;; backups
 '(make-backup-files t)
 '(backup-by-copying t)
 '(version-control t)
 '(delete-old-versions t)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(backup-directory-alist
   (list (cons "." (expand-file-name "var/backup/" user-emacs-directory))))

 '(auto-save-list-file-prefix
   (expand-file-name "var/auto-save/" user-emacs-directory))
 '(ring-bell-function #'ignore)
 '(visible-bell nil)
 '(ns-use-proxy-icon nil)
 '(enable-recursive-minibuffers t)
 '(recentf-max-saved-items 200)
 '(savehist-save-minibuffer-history t)
 '(savehist-additional-variables '(kill-ring
                                   mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring))
 '(history-length 20000)
 '(display-time-world-list '(("Asia/Taipei" "Taipei")
                             ("America/Toronto" "Toronto")
                             ("America/Los_Angeles" "San Francisco")
                             ("Europe/Berlin" "Düsseldorf")
                             ("Europe/London" "GMT")))
 '(hippie-expand-try-functions-list '(try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill))
 '(scroll-conservatively 101)           ; Don't recenter
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(ediff-split-window-function #'split-window-horizontally)
 '(shr-use-colors nil)
 '(shr-use-fonts nil)
 '(shr-indentation 0)
 '(shr-max-image-proportion 0.5)
 '(shr-image-animate nil)
 '(shr-width 72)
 '(shr-discard-aria-hidden t)
 '(shr-cookie-policy nil)

 '(column-number-mode t)
 '(show-paren-mode t)
 '(electric-pair-mode t)
 '(savehist-mode t)
 '(save-place-mode t)
 '(winner-mode t)
 '(global-so-long-mode t)
 '(delete-selection-mode t)

 ;; Emacs 29
 '(repeat-mode t)
 '(pixel-scroll-precision-mode t)
 '(show-paren-context-when-offscreen t))

(setq-default
 fill-column 80
 x-stretch-cursor t
 tab-width 8
 tab-always-indent t
 line-spacing 3
 indent-tabs-mode nil
 truncate-lines t
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

;;; Keybindings

(keymap-global-set "C-M-j" #'switch-to-buffer)
(keymap-global-set "C-M-<backspace>" #'backward-kill-sexp)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x k" #'kill-this-buffer)
(keymap-global-set "C-M-r" #'raise-sexp)
(keymap-global-set "M-h" nil)
(keymap-global-set "M-h w" #'mark-word)
(keymap-global-set "M-h SPC" #'mark-word)
(keymap-global-set "M-h s" #'mark-end-of-sentence)
(keymap-global-set "M-h p" #'mark-paragraph)
(keymap-global-set "M-h b" #'mark-end-of-buffer)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-i" #'delete-other-windows)
(keymap-global-set "M-SPC" #'cycle-spacing)
(keymap-global-set "C-M-/" #'dabbrev-completion)
(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "M-Z" #'zap-to-char)
(keymap-global-set "M-z" #'zap-up-to-char)
(keymap-global-set "s--" #'text-scale-decrease)
(keymap-global-set "s-<backspace>" #'kill-whole-line)
(keymap-global-set "s-=" #'text-scale-adjust)
(keymap-global-set "s-S" #'write-file)
(keymap-global-set "s-a" #'mark-whole-buffer)
(keymap-global-set "s-i" #'imenu)
(keymap-global-set "s-k" #'kill-this-buffer)
(keymap-global-set "s-p" #'project-find-file)
(keymap-global-set "s-q" #'save-buffers-kill-emacs)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-t" #'jump-to-scratch-buffer)
(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "s-v" #'yank)
(keymap-global-set "C-c f d" #'find-config)
(keymap-global-set "C-c f j" #'find-journal)
(keymap-global-set "C-c f i" #'find-inbox)
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
;; (keymap-global-set "C-<backspace>" #'kill-line-backwards)
(keymap-global-set "S-<return>" #'newline-at-end-of-line)
(keymap-global-set "C-x C-r" #'recentf-open-files+)
(keymap-global-set "C-w" #'backward-kill-word-or-region)
(keymap-global-set "M-Q" #'unfill-paragraph)
(keymap-global-set "M-q" #'fill-or-unfill-paragraph)
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

(defun jump-to-scratch-buffer ()
  "Jump to the existing *scratch* buffer or create a new one.

If the current buffer is the *scratch* buffer, create a new
scratch buffer.  The newly created buffer would have the
`buffer-confirm-kill' and `buffer-offer-save' protection."
  (interactive)
  (let ((existing-scratch-buf (get-buffer "*scratch*")))
    (if (and existing-scratch-buf
             (not (eq (current-buffer) existing-scratch-buf)))
        (switch-to-buffer existing-scratch-buf)
      (let ((new-scratch-buf (get-buffer-create
                              (generate-new-buffer-name"*scratch*"))))
        (switch-to-buffer new-scratch-buf)
        (with-current-buffer new-scratch-buf
          (setq-local buffer-confirm-kill t)
          (setq-local buffer-offer-save t)
          (not-modified))))))

(defun find-config ()
  (interactive)
  (find-file (expand-file-name user-init-file)))

(defun find-journal ()
  (interactive)
  (find-file (expand-file-name "~/org/journal/journal.org")))

(defun find-inbox ()
  (interactive)
  (find-file (expand-file-name "~/org/journal/inbox.org")))

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

(defun quick-edit ()
  "Util function for use with hammerspoon quick edit functionality."
  (interactive)
  (let ((qed-buffer (generate-new-buffer "*quick-edit*")))
    (switch-to-buffer qed-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (gfm-mode)))

(defun quick-edit-end ()
  "Util function to be executed on qed completion."
  (interactive)
  (mark-whole-buffer)
  (call-interactively 'kill-ring-save)
  (bury-buffer))

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

(defun kill-line-backwards ()
  "Kill line backwards."
  (interactive)
  (kill-line 0))

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

(defun transpose-windows ()
  "Swap the buffers shown in current and next window."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-window (next-window nil :no-minibuf nil)))
    (set-window-buffer nil (window-buffer next-window))
    (set-window-buffer next-window this-buffer)
    (select-window next-window)))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
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

(defun minibuffer-up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (delete-minibuffer-contents)))

(keymap-set minibuffer-local-filename-completion-map
            "C-<backspace>" #'minibuffer-up-directory)

;; https://www.emacswiki.org/emacs/HippieExpand#h5o-11
(defun hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
The optional argument can be generated with `make-hippie-expand-function'."
  (require 'cl)
  (let ((this-command 'hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (flet ((ding))   ; avoid the (ding) when hippie-expand exhausts its options.
          (while (progn
                   (funcall hippie-expand-function nil)
                   (setq last-command 'hippie-expand-completions)
                   (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish up
    ;; in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defun hippie-expand-completion-with (hippie-expand-function)
  "Offer completing-read using the specified hippie-expand function."
  (let* ((options (hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (completing-read "Hippie: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found for %s" he-substitute-string))))

(defun hippie-completion ()
  "Offer completing-read completion for the word at point."
  (interactive)
  (hippie-expand-completion-with 'hippie-expand))

(keymap-global-set "C-M-/" 'hippie-completion)

;;; Mac

(when (eq system-type 'darwin)
  (setq mac-right-command-modifier 'super
        mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier 'meta
        trash-directory "~/.Trash"
        insert-directory-program "/usr/local/bin/gls"
        dired-listing-switches "-aFGhlv --group-directories-first"))

;;; Theme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes/sketch-themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/sketch-themes/")

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

;;; Built-in Packages

(with-eval-after-load 'tab-bar
  (add-hook 'tab-bar-tab-post-open-functions
            (lambda (&rest _)
              (call-interactively #'tab-bar-rename-tab))))

(with-eval-after-load 'dired
  ;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (keymap-set global-map "C-x C-j" #'dired-jump)
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

  (keymap-set isearch-mode-map "C-o" #'isearch-occur)
  (keymap-set isearch-mode-map "C-<backspace>" #'isearch-delete-wrong)
  ;; DEL during isearch should edit the search string, not jump back to the
  ;; previous result
  (keymap-substitute isearch-mode-map #'isearch-delete-char #'isearch-del-char)

  (add-hook 'isearch-mode-end-hook 'isearch-exit-at-start))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/"))
(add-hook 'prog-mode-hook 'smartscan-mode)

;;; 3rd Party Packages

(with-eval-after-package-install 'orderless
  (custom-set-variables
   '(completion-styles '(orderless))
   '(completion-category-overrides '((file (styles partial-completion)))))
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

(with-eval-after-package-install 'corfu
  (corfu-global-mode))

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
  (setq prefix-help-command #'embark-prefix-help-command)
  (custom-set-variables
   '(embark-indicators '(embark-minimal-indicator
                         embark-highlight-indicator
                         embark-isearch-highlight-indicator))))

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
      (pop-to-buffer buffer-or-name)))

  (custom-set-variables
   '(helpful-switch-buffer-function #'helpful-switch-to-buffer)))

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

(custom-set-variables
 '(org-directory "~/org")
 '(org-hide-emphasis-markers t)
 '(org-ellipsis " …")
 '(org-special-ctrl-a/e t)
 '(org-src-fontify-natively t)
 '(org-src-tab-acts-natively t)
 '(org-src-window-setup 'current-window)
 '(org-cycle-separator-lines 2)
 '(org-edit-src-content-indentation 0)
 '(org-src-window-setup 'current-window)
 '(org-indirect-buffer-display 'current-window)
 '(org-hide-block-startup nil)
 '(org-src-preserve-indentation nil)
 '(org-adapt-indentation nil)
 '(org-startup-folded 'content)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-image-actual-width 640)
 '(org-attach-auto-tag "attachment")
 '(org-agenda-files '("~/org/journal/inbox.org"
                      "~/org/journal/journal.org"))
 '(org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "REVIEW(r)" "|" "DONE(d!)")
                       (sequence "WAIT(w)" "|" "BACK(b)" "DECLINED(D)")))
 '(org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                            ("NEXT" :foreground "blue" :weight bold)
                            ("REVIEW" :foreground "orange" :weight bold)
                            ("WAIT" :foreground "HotPink2" :weight bold)
                            ("BACK" :foreground "MediumPurple3" :weight bold)
                            ("DECLINED" :foreground "forest green" :weight bold)
                            ("DONE" :foreground "forest green" :weight bold)))
 '(org-agenda-window-setup 'current-window)
 '(org-agenda-span 'day)
 '(org-agenda-start-with-log-mode t)
 '(org-default-notes-file "~/org/journal/inbox.org")
 '(org-capture-templates '(("t" "todo" entry
                            (file "~/org/journal/inbox.org")
                            "* TODO %?\n"
                            :clock-in t
                            :clock-resume t
                            :empty-lines 1)
                           ("j" "journal" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* %?\n%i\n%a"
                            :tree-type week
                            :empty-lines 1)
                           ("i" "check in" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* Check in %U\n#+BEGIN: clocktable :scope agenda :maxlevel 4 :tcolumns 1 :block %<%Y-%m-%d>\n#+END:"
                            :tree-type week
                            :immediate-finish t)
                           ("o" "check out" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* Check out - %U"
                            :tree-type week
                            :immediate-finish t)
                           ("m" "meeting" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* %^{Meeting} :meeting:\n%?\n"
                            :tree-type week
                            :clock-in t
                            :clock-resume t
                            :empty-lines 1)
                           ("l" "learning" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* %? :learning:\n%U\n"
                            :tree-type week
                            :clock-in t
                            :clock-resume t
                            :empty-lines 1)
                           ("s" "slack" entry
                            (file+olp+datetree "~/org/journal/journal.org")
                            "* Slack :meeting:\n%?\n"
                            :tree-type week
                            :clock-in t
                            :clock-resume t
                            :empty-lines 1)))
 '(org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
 '(org-refile-use-outline-path t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes 'confirm))

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," nil)

  (require 'ox-md)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(with-eval-after-package-install 'org-roam
  (setq org-roam-v2-ack t)
  (custom-set-variables
   '(org-roam-directory "~/org/roam/"))
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

(with-eval-after-package-install 'magit
  (autoload #'magit-project-status "magit" nil t)
  (keymap-global-set "s-g" #'magit-status)
  (keymap-global-set "C-x g" #'magit-status)
  (keymap-global-set "C-c g" #'magit-file-dispatch)
  (custom-set-variables
   '(magit-diff-refine-hunk 'all)
   '(magit-display-buffer-function
     #'magit-display-buffer-same-window-except-diff-v1)))

(with-eval-after-package-install 'rg
  (keymap-global-set "s-F" #'rg-project)
  (keymap-global-set "C-c r" #'rg)
  (with-eval-after-load 'rg
    (rg-enable-default-bindings)))

(with-eval-after-package-install 'flycheck
  (add-hook 'prog-init-hook 'flycheck-mode))

;; (with-eval-after-package-install 'lsp-mode
;;   (add-hook 'clojure-mode-hook 'lsp)
;;   (add-hook 'clojurec-mode-hook 'lsp)
;;   (add-hook 'clojurescript-mode-hook 'lsp)
;;   (custom-set-faces
;;    `(lsp-face-highlight-textual ((t (:inherit lazy-highlight)))))
;;   (custom-set-variables
;;    '(lsp-enable-file-watchers nil)
;;    '(lsp-headerline-breadcrumb-enable nil)
;;    '(lsp-keymap-prefix "C-c L")
;;    '(lsp-enable-indentation nil)
;;    '(lsp-enable-snippet nil)
;;    '(lsp-completion-provider :none)
;;    '(lsp-eldoc-enable-hover nil)
;;    '(lsp-modeline-diagnostics-scope :file)
;;    '(lsp-modeline-code-actions-enable nil)
;;    '(lsp-lens-enable nil))
;;   (setq read-process-output-max (* 1024 1024))

;;   (defun lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless)))
;;   (add-hook 'lsp-completion-mode #'lsp-mode-setup-completion))

(with-eval-after-package-install 'eglot
  (add-hook 'clojure-mode-hook 'eglot-ensure)
  (add-hook 'clojurec-mode-hook 'eglot-ensure)
  (add-hook 'clojurescript-mode-hook 'eglot-ensure))

;;; Language major modes

(with-eval-after-package-install 'clojure-mode
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local hippie-expand-try-functions-list
                          '(try-expand-dabbrev
                            try-expand-dabbrev-all-buffers
                            try-expand-dabbrev-from-kill))))

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
        (-kill-and-echo (format "%s/%s" ns (cadr def)))))))

(with-eval-after-package-install 'cider
  (custom-set-variables
   '(cider-repl-display-help-banner nil)
   '(cider-repl-display-in-current-window nil)
   '(cider-repl-pop-to-buffer-on-connect nil)
   '(cider-repl-use-pretty-printing t)
   '(cider-repl-buffer-size-limit 100000)))

(with-eval-after-package-install 'markdown-mode
  (add-hook 'markdown-mode-hook 'auto-fill-mode)
  (setq markdown-command "marked"))

(with-eval-after-package-install 'flyspell
  (with-eval-after-load 'flyspell
    (keymap-set flyspell-mode-map "C-." nil)  ; Reserved for embark-act
    (keymap-set flyspell-mode-map "C-;" nil)) ; Reserved for iedit
  (add-hook 'text-mode-hook 'flyspell-mode))

(with-eval-after-package-install 'emmet-mode
  (add-hook 'html-mode 'emmet-mode)
  (add-hook 'css-mode 'emmet-mode))

(with-eval-after-package-install 'sly
  (setq inferior-lisp-program "sbcl"))

(with-eval-after-package-install 'sqlformat
  (custom-set-variables
   '(sqlformat-command 'pgformatter)
   '(sqlformat-args '("-s2" "-g")))
  (with-eval-after-load 'sql
    (keymap-set sql-mode-map "C-c C-f" #'sqlformat-buffer)))

(with-eval-after-package-install 'elfeed
  (autoload #'elfeed "elfeed" nil t)
  (custom-set-variables
   '(elfeed-feeds '("http://irreal.org/blog/?feed=rss2"
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
                    "https://www.with-emacs.com/rss."))))

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
