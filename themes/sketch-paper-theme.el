;;; sketch-paper-theme.el --- Sketch Paper Theme

;; Copyright (C) 2023 Daw-Ran Liou

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d/themes
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
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

;; The Sketch Paper color theme.

;;; Code:
(deftheme sketch-paper)

(let ((class '((class color) (min-colors 89)))
      (fg "#212121")
      (bg "#eee1c4")
      (weak "#A19274")
      (highlight-1 "#F0CC7D")
      (highlight-2 "#FFF9EB")
      (highlight-3 "#C0B7ED")
      (highlight-4 "#C2D2ED")
      (success "#87a985")
      (warning "#c87e7e"))
  (custom-theme-set-faces
   'sketch-paper
   ;; default
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(fringe ((,class (:background ,bg :foreground ,weak))))
   `(shadow ((,class (:inherit fixed-pitch))))
   `(highlight ((,class (:background ,highlight-4))))
   `(region ((,class (:foreground ,fg :background ,highlight-1))))
   `(show-paren-match ((,class (:background ,highlight-1))))
   `(show-paren-mismatch ((,class (:background ,warning))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,fg))))
   `(isearch ((,class (:bold t :foreground ,fg :background ,highlight-1 :bold t))))
   `(lazy-highlight ((,class (:foreground ,fg :background ,highlight-2))))
   `(link ((,class (:underline t))))
   `(parenthesis ((,class (:foreground ,weak))))
   `(trailing-whitespace ((,class (:foreground nil :background ,warning))))
   `(cursor ((,class (:background ,fg :foreground ,bg))))
   `(vertical-border ((,class (:foreground ,fg))))
   `(default-italic ((,class (:italic t))))
   `(line-number ((,class (:background ,bg :foreground ,weak))))
   `(line-number-current-line ((,class (:background ,bg :foreground ,fg))))

   ;; mode line
   `(mode-line ((,class (:foreground ,fg :box ,fg))))
   `(mode-line-inactive ((,class (:foreground ,weak :weight light :box ,fg))))

   ;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,fg))))
   `(font-lock-comment-face ((,class (:inherit font-lock-string-face))))
   `(font-lock-negation-char-face ((,class (:foreground ,fg))))
   `(font-lock-reference-face ((,class (:foreground ,fg))))
   `(font-lock-constant-face ((,class (:bold t))))
   `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :bold t))))
   `(font-lock-keyword-face ((,class (:foreground ,fg))))
   `(font-lock-string-face ((,class (:foreground ,weak))))
   `(font-lock-type-face ((,class (:foreground ,fg))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg :bold t))))
   `(font-lock-warning-face ((,class (:underline (:color ,warning :style wave)))))
   `(fill-column-indicator ((,class (:foreground ,weak))))

   ;; hl line
   `(hl-line ((,class (:background ,highlight-2))))

   ;; Alternative background
   `(magit-section-highlight ((,class (:background ,highlight-2))))))

(provide 'sketch-paper-theme)
;;; sketch-paper-theme.el ends here
