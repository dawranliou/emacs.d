;;; bsod-theme.el --- BSoD (Blue Screen of Death) Theme

;; Copyright (C) 2023 Daw-Ran Liou

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d/themes
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; The BSoD (Blue Screen of Death) color theme.

;;; Code:
(deftheme bsod)

(let ((class '((class color) (min-colors 89)))
      (black "#000000")
      (blue-shade "#0000d8")
      (blue "#0000ff")
      (red-shade "#d80000")
      (red "#ff0000")
      (magenta-shade "#d800d8")
      (magenta "#ff00ff")
      (green-shade "#00d800")
      (green "#00ff00")
      (cyan-shade "#00d8d8")
      (cyan "#00ffff")
      (yellow-shade "#d8d800")
      (yellow "#ffff00")
      (shade "#d8d8d8")
      (white "#ffffff"))
  (custom-theme-set-faces
   'bsod
   ;; default
   `(default ((,class (:background ,blue-shade :foreground ,white))))
   `(fringe ((,class (:background ,blue-shade :foreground ,shade))))
   `(shadow ((,class (:inherit fixed-pitch))))
   `(highlight ((,class (:foreground ,green :inverse-video t))))
   `(region ((,class (:foreground ,yellow :inverse-video t))))
   `(show-paren-match ((,class (:foreground ,green-shade :bold t :inverse-video t))))
   `(show-paren-mismatch ((,class (:foreground ,red-shade :bold t :inverse-video t))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,white))))
   `(isearch ((,class (:bold t :inverse-video t :foreground ,magenta :bold t))))
   `(lazy-highlight ((,class (:inverse-video t :foreground ,cyan-shade))))
   `(link ((,class (:underline t))))
   ;; `(parenthesis ((,class (:foreground ,shade))))
   `(trailing-whitespace ((,class (:foreground nil :inverse-video t :foreground ,red-shade))))
   ;; `(cursor ((,class (:background ,green))))
   `(vertical-border ((,class (:foreground ,white))))
   `(default-italic ((,class (:italic t))))
   `(line-number ((,class (:foreground ,shade))))
   `(line-number-current-line ((,class (:foreground ,shade :inverse-video t))))
   `(hl-line ((,class (:foreground ,green :inverse-video t))))
   ;; `(diff-refine-added ((,class (:background ,green))))
   ;; `(diff-refine-removed ((,class (:background ,red))))

   ;; mode line
   `(mode-line ((,class (:foreground ,shade :inverse-video t :box (:style released-button)))))
   `(mode-line-inactive ((,class (:foreground ,shade :weight light :box (:style released-button)))))

   ;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,white))))
   `(font-lock-comment-face ((,class (:foreground ,cyan-shade))))
   `(font-lock-negation-char-face ((,class (:foreground ,white))))
   `(font-lock-reference-face ((,class (:foreground ,white))))
   `(font-lock-constant-face ((,class (:bold t))))
   `(font-lock-doc-face ((,class (:foreground ,cyan))))
   `(font-lock-function-name-face ((,class (:foreground ,white :bold t))))
   `(font-lock-keyword-face ((,class (:foreground ,white))))
   `(font-lock-string-face ((,class (:foreground ,magenta))))
   `(font-lock-type-face ((,class (:foreground ,white))))
   `(font-lock-variable-name-face ((,class (:foreground ,white :bold t))))
   `(font-lock-warning-face ((,class (:underline (:color ,red-shade :style wave)))))
   `(fill-column-indicator ((,class (:foreground ,shade))))

   ;; Clojure mode
   `(clojure-keyword-face ((,class (:foreground ,white))))

   ;; Magit
   ;; `(magit-diff-hunk-heading-highlight ((,class (:foreground ,shade))))
   ;; `(magit-diff-context ((,class (:foreground ,shade))))
   ;; `(magit-diff-hunk-region ((,class (:background ,shade))))
   ;; `(magit-diff-hunk-heading-highlight ((,class (:inverse-video t :foreground ,yellow-shade))))
   ;; `(magit-diff-context-highlight ((,class (:inverse-video t :foreground ,cyan-shade))))
   ;; `(magit-diff-added-highlight ((,class (:background ,green-shade))))
   ;; `(magit-diff-removed-highlight ((,class (:background ,red-shade))))
   ;; `(magit-diff-added ((,class (:background ,green-shade))))
   ;; `(magit-diff-removed ((,class (:background ,red-shade))))
   `(magit-section-highlight ((,class (:background ,cyan-shade))))
   ))

(provide 'bsod-theme)
;;; bsod-theme.el ends here
