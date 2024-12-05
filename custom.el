(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("14436a10b0cb5b7b6e6f6d490a08c1a751ad0384e9b124b9b8d5d554129f5571" default))
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
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (awk . t)
     (shell . t)
     (clojure . t)))
 '(org-capture-templates
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
 '(org-clock-clocked-in-display nil)
 '(org-clock-clocktable-default-properties '(:maxlevel 4))
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-hide-block-startup nil)
 '(org-cycle-separator-lines 2)
 '(org-default-notes-file "~/org/journal/inbox.org")
 '(org-directory "~/org")
 '(org-ellipsis " ⤵ ")
 '(org-export-with-sub-superscripts '{})
 '(org-fontify-done-headline nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-block-startup nil)
 '(org-hide-emphasis-markers t)
 '(org-image-actual-width '(640))
 '(org-indirect-buffer-display 'current-window)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
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
 '(org-todo-keyword-faces
   '(("NEXT" . "blue")
     ("REVIEW" . "dark orange")
     ("HOLD" . "purple")
     ("CANCELLED" . "teal")))
 '(org-todo-keywords
   '((sequence "TODO(t@/)" "NEXT(n)" "REVIEW(r@/!)" "|" "DONE(d!)")
     (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-use-speed-commands t)
 '(package-native-compile t)
 '(package-selected-packages
   '(esup eglot gotest dape nerd-icons-dired nerd-icons-completion nerd-icons-corfu nerd-icons dired-subtree multiple-cursors ox-gfm avy cape casual-dired cider clojure-mode clojure-ts-mode clojure-ts-mode consult corfu csv-mode docker dockerfile-mode dumb-jump eat edit-indirect eglot-booster embark embark-consult fennel-mode flyspell gnuplot go-mode groovy-mode helpful hide-mode-line iedit jarchive jdecomp jinx kotlin-mode lua-mode magit marginalia markdown-mode markdown-toc ob-restclient orderless org pulsar rainbow-mode restclient rg sly sqlformat standard-themes tb-keycast verb vertico websocket which-key ws-butler yaml-mode zig-mode))
 '(package-vc-selected-packages
   '((go-mode :vc-backend Git :url "https://github.com/dominikh/go-mode.el")
     (tb-keycast :vc-backend Git :url "https://github.com/ir33k/tb-keycast.git")
     (eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")
     (clojure-ts-mode :url "https://github.com/clojure-emacs/clojure-ts-mode" :vc-backend Git))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-highlight-symbol-face ((t (:inherit lazy-highlight))))
 '(elisp-shorthand-font-lock-face ((t (:inherit font-lock-keyword-face :weight bold))))
 '(org-block ((t (:inherit fixed-pitch :extend t))))
 '(org-checkbox ((t (:inherit (bold fixed-pitch)))))
 '(org-date ((t (:inherit fixed-pitch :foreground "Purple" :underline t))))
 '(org-document-info-keyword ((t (:inherit fixed-pitch))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-ellipsis ((t (:underline t))))
 '(org-hide ((t (:inherit fixed-pitch :foreground "White"))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-keyword-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch))))
 '(tab-bar ((t (:inherit fixed-pitch :background "grey85" :foreground "black"))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "White" :box (:line-width (2 . 2) :color "White")))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "grey75" :box (:line-width (2 . -2) :color "grey75"))))))
