;;; duo-theme.el --- Theme Duo

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

;;; Code:

(deftheme duo
  "Duo color theme.")

(let ((class '((class color) (min-colors 89)))
      (fg         "#ECEFF4")                   ;; Snow Storm 3  / nord  6
      (bg         "#2E3440")                   ;; Polar Night 0 / nord  0
      (highlight  "#3B4252")                   ;; Polar Night 1 / nord  1
      (critical   "#EBCB8B")                   ;; Aurora        / nord 11
      (salient    "#81A1C1")                   ;; Frost         / nord  9
      (strong     "#ECEFF4")                   ;; Snow Storm 3  / nord  6
      (popout     "#D08770")                   ;; Aurora        / nord 12
      (subtle     "#434C5E")                   ;; Polar Night 2 / nord  2
      (faded      "#677691")                   ;;
      ;; (fg         "#B6AAEE")
      ;; (bg         "#2A2833")
      ;; (highlight  "#3A3A4A")
      ;; (critical   "#FFB375")
      ;; (salient    "#BEBEEF")
      ;; (strong     "#BEBEEF")
      ;; (popout     "#B06845")
      ;; (subtle     "#49495A")
      ;; (faded      "#7272A1")
      )
  (custom-theme-set-faces
   'duo
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(region ((,class (:background ,subtle))))
   `(highlight ((,class (:background ,subtle))))
   `(cursor ((,class (:background ,fg))))

   `(window-divider ((,class (:foreground ,bg))))
   `(vertical-border ((,class (:foreground ,subtle :background ,bg))))
   `(internal-border ((,class (:background ,bg))))
   `(fringe ((,class (:foreground ,subtle :background ,bg))))
   `(buffer-menu-buffer ((,class (:foreground ,strong :bold t))))
   `(minibuffer-prompt ((,class (:foreground ,strong :bold t))))
   `(link ((,class (:foreground ,salient))))
   `(isearch ((,class (:foreground ,strong :bold t))))
   `(isearch-fail ((,class (:foreground ,faded))))
   `(lazy-highlight ((,class (:background ,subtle))))
   `(trailing-whitespace ((,class (:background ,subtle))))
   `(show-paren-match ((,class (:foreground ,popout))))
   `(line-number ((,class (:foreground ,faded))))
   `(hl-line ((,class (:background ,highlight))))

   ;; Semantic
   `(shadow ((,class (:foreground ,faded))))
   `(success ((,class (:foreground ,salient))))
   `(warning ((,class (:foreground ,popout))))
   `(error ((,class (:foreground ,fg :background ,critical))))
   `(match ((,class (:foreground ,popout))))

   ;; Mode line
   `(mode-line ((,class (:overline ,subtle))))
   `(mode-line-inactive ((,class (:foreground ,subtle))))

   ;; font-lock
   `(font-lock-builtin-face ((,class (:foreground ,salient))))
   `(font-lock-comment-face ((,class (:foreground ,faded))))
   `(font-lock-constant-face ((,class (:foreground ,salient))))
   `(font-lock-doc-face ((,class (:foreground ,faded))))
   `(font-lock-function-name-face ((,class (:foreground ,strong :bold t))))
   `(font-lock-keyword-face ((,class (:foreground ,salient))))
   `(font-lock-string-face ((,class (:foreground ,popout))))
   `(font-lock-warning-face ((,class (:foreground ,popout))))
   `(font-lock-type-face ((,class (:foreground ,salient ))))
   `(font-lock-variable-name-face ((,class (:foreground ,strong :bold t))))

   ;; Org
   `(org-archived ((,class (:foreground ,faded))))
   `(org-block ((,class (:inherit (fixed-pitch hl-line)))))
   `(org-block-begin-line ((,class (:foreground ,faded))))
   `(org-block-end-line ((,class (:foreground ,faded))))
   ;; `(org-block nil                      :extend t)
   ;; `(org-block-begin-line nil           :extend t)
   ;; `(org-block-end-line nil             :extend t)
   `(org-checkbox ((,class (:foreground ,faded))))
   `(org-checkbox-statistics-done ((,class (:foreground ,faded))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,faded))))
   `(org-clock-overlay ((,class (:foreground ,faded))))
   `(org-code ((,class (:foreground ,faded))))
   `(org-column ((,class (:foreground ,faded))))
   `(org-column-title ((,class (:foreground ,faded))))
   `(org-date ((,class (:foreground ,faded))))
   `(org-date-selected ((,class (:foreground ,faded))))
   `(org-default ((,class (:foreground ,faded))))
   `(org-document-info ((,class (:foreground ,faded))))
   `(org-document-info-keyword ((,class (:foreground ,faded))))
   `(org-document-title ((,class (:foreground ,faded))))
   `(org-done ((,class (:inherit default))))
   `(org-drawer ((,class (:foreground ,faded))))
   `(org-ellipsis ((,class (:foreground ,faded))))
   `(org-footnote ((,class (:foreground ,faded))))
   `(org-formula ((,class (:foreground ,faded))))
   `(org-headline-done ((,class (:foreground ,faded))))
   ;; `(org-hide ((,class (:foreground ,faded))))
   ;; `(org-indent ((,class (:foreground ,faded))))
   `(org-latex-and-related ((,class (:foreground ,faded))))
   `(org-level-1 ((,class (:foreground ,strong :bold t))))
   `(org-level-2 ((,class (:foreground ,strong :bold t))))
   `(org-level-3 ((,class (:foreground ,strong :bold t))))
   `(org-level-4 ((,class (:foreground ,strong :bold t))))
   `(org-level-5 ((,class (:foreground ,strong :bold t))))
   `(org-level-6 ((,class (:foreground ,strong :bold t))))
   `(org-level-7 ((,class (:foreground ,strong :bold t))))
   `(org-level-8 ((,class (:foreground ,strong :bold t))))
   `(org-link ((,class (:foreground ,salient))))
   `(org-list-dt ((,class (:foreground ,faded))))
   `(org-macro ((,class (:foreground ,faded))))
   `(org-meta-line ((,class (:foreground ,faded))))
   `(org-mode-line-clock ((,class (:foreground ,faded))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,faded))))
   `(org-priority ((,class (:foreground ,faded))))
   `(org-property-value ((,class (:foreground ,faded))))
   `(org-quote ((,class (:foreground ,faded))))
   `(org-scheduled ((,class (:foreground ,faded))))
   `(org-scheduled-previously ((,class (:foreground ,faded))))
   `(org-scheduled-today ((,class (:foreground ,faded))))
   `(org-sexp-date ((,class (:foreground ,faded))))
   `(org-special-keyword ((,class (:foreground ,faded))))
   `(org-table ((,class (:foreground ,faded))))
   `(org-tag ((,class (:foreground ,faded))))
   `(org-tag-group ((,class (:foreground ,faded))))
   `(org-target ((,class (:foreground ,faded))))
   `(org-time-grid ((,class (:foreground ,faded))))
   `(org-todo ((,class (:foreground ,salient))))
   `(org-upcoming-deadline ((,class (:foreground ,faded))))
   `(org-verbatim ((,class (:foreground ,popout))))
   `(org-verse ((,class (:foreground ,faded))))
   `(org-warning ((,class (:foreground ,popout))))

   ;; Magit
   `(magit-section-highlight ((,class (:background ,highlight))))
   `(magit-diff-highlight ((,class (:background ,highlight))))
   `(magit-diff-context-highlight ((,class (:background ,highlight))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,subtle))))
   `(magit-diff-hunk-heading ((,class (:background ,subtle))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'duo)

(provide 'duo-theme)
;;; vs-dark-theme.el ends here
