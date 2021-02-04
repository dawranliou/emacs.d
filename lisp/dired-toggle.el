;;; dired-toggle.el --- Toggle a dired window  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Daw-Ran Liou

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dired)

(defvar dired-toggle-mode-map
  (let ((map (make-sparse-keymap)))
    (with-eval-after-load 'dired-subtree
      (define-key map [tab] 'dired-subtree-toggle))
    (with-eval-after-load 'evil
      (evil-define-key 'normal map [tab] 'dired-subtree-toggle))
    map)
  "Keymap used in `dired-toggle-mode'.")

(define-minor-mode dired-toggle-mode
  "A minor mode that shows dired as a side window."
  :init-value nil
  :lighter ""
  :keymap dired-toggle-mode-map

  (dired-hide-details-mode)
  (dired-toggle--set-mode-line))

(defun dired-toggle--set-mode-line ()
  "Set up mode-line."
  (setq mode-line-format '("%e" mode-line-front-space
                           mode-line-buffer-identification
                           "  " (vc-mode vc-mode) "  "  mode-line-end-spaces)))

;;;###autoload
(defun dired-toggle (&optional use-project-root)
  "Open a dired-toggle."
  (interactive "P")
  (let* ((path (if use-project-root
                   (project-root (project-current))
                 default-directory))
         (name (abbreviate-file-name path))
         (buffer (or (get-buffer name)
                     (dired-noselect path))))
    ;; Rename dired buffer to name
    (when (not (string-equal (buffer-name buffer) name))
      (with-current-buffer buffer
        (rename-buffer name)))

    ;; Open/close window
    (if-let ((window (get-buffer-window buffer)))
        (delete-window window)
      (display-buffer-in-side-window buffer '((side . left)))
      (set-window-dedicated-p (get-buffer-window buffer) t)
      (with-current-buffer buffer
        (dired-toggle-mode)))))

(provide 'dired-toggle)
;;; dired-toggle.el ends here
