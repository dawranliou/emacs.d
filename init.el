;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar dawran/default-font-size 140)
(defvar dawran/default-variable-font-size 160)

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

(use-package general
  :config
  (general-create-definer dawran/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  (general-create-definer dawran/localleader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :major-modes t
    :prefix ","
    :non-normal-prefix "M-,")

  (dawran/leader-keys
    "fd" '((lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/README.org"))) :which-key "edit config")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "tw" 'whitespace-mode))

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

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; No beeping nor visible bell
(setq ring-bell-function #'ignore
      visible-bell nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'sketch-white t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(blink-cursor-mode 0)

(hl-line-mode 1)

(set-face-attribute 'default nil :font "Monolisa" :height dawran/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Monolisa" :height dawran/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height dawran/default-variable-font-size :weight 'regular)

(use-package doom-themes
  ;:init (load-theme 'doom-palenight t)
)

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-lsp t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :init
  (ivy-mode 1)
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
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

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "+")
  ("k" text-scale-decrease "-")
  ("f" nil "finished" :exit t))

(dawran/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package paren-face
  :hook
  (lispy-mode . paren-face-mode))

(defun dawran/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Monolisa" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun dawran/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (dawran/org-font-setup))

(use-package org
  :hook (org-mode . dawran/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun dawran/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dawran/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
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

;; TODO add org-roam

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
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :hook (eshell-first-time-mode . dawran/configure-eshell))

(dawran/leader-keys
  "SPC" 'eshell)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay nil))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :bind (("s-F" . counsel-projectile-rg)
         ("s-p" . counsel-projectile))
  :config
  (counsel-projectile-mode)
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-vc))))

(dawran/leader-keys
  "pf"  'counsel-projectile-find-file
  "ps"  'counsel-projectile-switch-project
  "pF"  'counsel-projectile-rg
  "pp"  'counsel-projectile
  "pd"  'projectile-dired)

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

(dawran/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gg"  'magit-status
  "gd"  'magit-diff-unstaged
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package expand-region
  :bind ("s-'" .  er/mark-outside-pairs))

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

(use-package lsp-mode
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
        lsp-enable-indentation nil))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (clojurescript-mode . lispy-mode)
         (cider-repl-mode . lispy-mode))
  :config
  (add-hook 'lispy-mode-hook (lambda () (modify-syntax-entry ?- "w"))))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators
                              c-w
                              additional
                              additional-insert
                              additional-movement
                              (atom-movement normal visual)
                              slurp/barf-cp)))

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
    "ee" 'cider-eval-last-sexp))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))
