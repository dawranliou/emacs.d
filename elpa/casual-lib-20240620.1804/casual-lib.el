;;; casual-lib.el --- Library routines for Casual porcelains -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-lib
;; Keywords: tools
;; Version: 1.0.1
;; Package-Requires: ((emacs "29.1"))

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

;; Library routines for Casual porcelains.

;;; Code:
(require 'transient)
(require 'casual-lib-version)

(defcustom casual-lib-use-unicode nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'casual)

(defun casual-lib-customize-casual-lib-use-unicode ()
  "Customize `casual-lib-use-unicode'.

Customize Casual porcelains to use Unicode symbols in place of strings
when appropriate."
  (interactive)
  (customize-variable 'casual-lib-use-unicode))

(defun casual-lib-unicode-db-get (key db)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.
- DB alist containing Unicode symbol map.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (let* ((db db)
         (unicode casual-lib-use-unicode)
         (item (alist-get key db)))
    (if unicode
        (nth 0 (eval item))
      (nth 1 (eval item)))))

;; Predicates
(defun casual-lib-display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (if display-line-numbers t nil))

(defun casual-lib-buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun casual-lib-buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (casual-lib-buffer-writeable-p) (region-active-p)))

;; Labels
(defun casual-lib--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if casual-lib-use-unicode
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

(defun casual-lib--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

(defun casual-lib-checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (casual-lib--prefix-label label (casual-lib--variable-to-checkbox v)))

(defun casual-lib--suffix-label (label suffix)
  "Label constructed with LABEL and SUFFIX separated by a space."
  (format "%s %s" label suffix))

;; Transient Navigation
(transient-define-suffix casual-lib-quit-all ()
  "Dismiss all menus."
  :transient nil
  :key "C-q"
  :description "Dismiss"
  (interactive)
  (transient-quit-all))

(transient-define-suffix casual-lib-quit-one ()
  "Go back to previous menu."
  :transient nil
  :key "C-g"
  :description "‹Back"
  (interactive)
  (transient-quit-one))

(provide 'casual-lib)
;;; casual-lib.el ends here
