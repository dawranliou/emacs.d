;;; oil6-theme.el --- Theme Oil 6

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d/themes
;; Version: 0.1
;; Package-Requires: ((emacs "27"))
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

;; A theme based on the Oil 6 Palette created by GrafxKid
;; https://lospec.com/palette-list/oil-6. The palette has the following six
;; colours: #fbf5ef #f2d3ab #c69fa5 #8b6d9c #494d7e #272744

;;; Code:

(deftheme oil6)
(let ((class '((class color) (min-colors 89)))
      (fg1 "#fbf5ef")
      (fg2 "#f2d3ab")
      (fg3 "#c69fa5")
      (fg4 "#8b6d9c")
      (bg1 "#272744")
      (bg2 "#494d7e")
      (bg3 "#8b6d9c")
      (bg4 "#c69fa5")
      (builtin "#fbf5ef")
      (keyword "#fbf5ef")
      (const   "#fbf5ef")
      (comment "#8b6d9c")
      (func    "#f2d3ab")
      (str     "#c69fa5")
      (type    "#fbf5ef")
      (var     "#f2d3ab")
      (warning "#8b6d9c")
      (warning2 "#f2d3ab"))
  (custom-theme-set-faces
   'oil6
   ;; Default
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(default-italic ((,class (:italic t))))

   ;; Emacs UI
   `(region ((,class (:foreground ,fg1 :background ,bg3))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
   `(cursor ((,class (:background ,bg4))))
   `(header-line ((,class (:inherit mode-line))))
   `(mode-line ((,class (:box (:line-width 5 :color ,bg2) :foreground ,fg3 :background ,bg2))))
   `(mode-line-inactive ((,class (:box (:line-width 5 :color ,bg1) :foreground ,fg4 :background ,bg1))))
   `(mode-line-buffer-id ((,class (:bold t :foreground ,fg2))))
   `(line-number ((t (:inherit fringe :background ,bg1 :foreground ,fg4))))
   `(line-number-current-line ((t (:inherit fringe :foreground ,fg2 :weight bold))))
   `(link ((,class (:foreground ,const :underline t))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
   `(vertical-border ((,class (:foreground ,fg3))))
   `(warning ((,class (:foreground ,warning))))

   ;; Language Syntax highlighting
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:underline (:color ,warning :style wave)))))

   ;; Highlighting
   `(highlight ((,class (:foreground ,fg1 :background ,bg3))))
   `(hl-line ((,class (:background ,bg2))))
   `(idle-highlight ((,class (:background ,bg2))))
   `(hl-fill-column-face ((,class (:background ,bg2))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(show-paren-match ((,class (:background ,bg3))))
   `(isearch ((,class (:inherit region))))
   `(lazy-highlight ((,class (:inherit region))))

   ;; Flymake
   `(flymake-warning ((,class (:underline (:style wave :color ,warning)))))
   `(flymake-error ((,class (:underline (:style wave :color ,warning2)))))

   ;; Org
   `(org-document-title ((,class (:foreground ,fg2 :height 1.4 :bold t))))
   `(org-hide ((,class (:foreground ,bg1))))
   `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.3))))
   `(org-level-2 ((,class (:bold nil :foreground ,fg3 :height 1.2))))
   `(org-level-3 ((,class (:bold t :foreground ,fg4 :height 1.1))))
   `(org-level-4 ((,class (:bold nil :foreground ,bg4 :height 1.1))))
   `(org-level-5 ((,class (:bold nil :foreground ,bg4 :height 1.1))))
   `(org-level-6 ((,class (:bold nil :foreground ,bg4 :height 1.1))))
   `(org-level-7 ((,class (:bold nil :foreground ,bg4 :height 1.1))))
   `(org-level-8 ((,class (:bold nil :foreground ,bg4 :height 1.1))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-special-keyword ((,class (:foreground ,func :inherit (fixed-pitch)))))
   `(org-meta-line ((,class (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-checkbox ((,class (:inherit fixed-pitch))))
   `(org-block ((,class (:foreground ,fg1 :background ,bg2 :inherit fixed-pitch :extend t))))
   `(org-code ((,class (:inherit org-block :foreground ,fg2))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:foreground ,keyword :bold t))))
   `(org-table ((,class (:inherit fixed-pitch))))
   `(org-done ((,class (:bold t :foreground ,bg4))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg2 :background ,bg2 :inherit fixed-pitch))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(org-sexp-date ((,class (:foreground ,fg4))))

   ;; Clojure
   `(clojure-keyword-face ((,class (:foreground ,var))))
   `(cider-result-overlay-face ((,class (:background ,bg2))))

   ;; Info
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))

   ;; Magit
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-section-heading        ((,class (:foreground ,func :weight bold))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-diff-context-highlight ((,class (:background ,bg2))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'oil6)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; oil6-theme.el ends here
