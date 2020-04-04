;;; init.el -- An emacs configuration by Daw-Ran Liou

;;; Commentary:

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visible-bell t)

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package evil
  :ensure t
  :custom
  (evil-move-beyond-eol t)
  :config
  (evil-mode t))

(use-package counsel
  :ensure t
  :config
  (counsel-mode t))

(use-package swiper
  :ensure t
  :commands swiper
  :bind (("C-s" . counsel-grep-or-swiper)
	 :map evil-motion-state-map
	      ("/" . swiper)
	      ("?" . swiper-backward)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode t))

(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t))

(use-package rg
  :ensure t
  :commands rg)

(use-package magit
  :ensure t
  :defer t
  :bind
  ("C-c g s" . magit-status))

;;; Init.el ends here
