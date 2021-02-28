(deftheme en4
  "A four-color theme.")

(let ((class '((class color) (min-colors 89)))
      (fg "#d5e6cb")
      (bg "#1b1b1b")
      ;; (pop "#e7bb8b")
      ;; (dim "#e7bb8b")
      (hl "#564295"))
  (custom-theme-set-faces
   'en4
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,hl))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,bg))))
   `(highlight ((,class (:background ,hl))))
   `(region ((,class (:background ,hl))))
   `(secondary-selection ((,class (:background ,hl :foreground ,fg))))
   `(isearch ((,class (:weight bold))))
   `(lazy-highlight ((,class (:background ,hl))))
   ;; Mode line faces
   `(mode-line ((,class (:background ,bg :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,hl))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,fg))))
   `(escape-glyph ((,class (:foreground ,fg :weight bold))))
   `(homoglyph ((,class (:foreground ,fg :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,fg))))
   `(font-lock-comment-face ((,class (:weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,fg))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,fg))))
   `(font-lock-string-face ((,class (:weight bold))))
   `(font-lock-type-face ((,class (:foreground ,fg))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg :weight bold))))
   `(font-lock-warning-face ((,class (:weight bold))))
   ;; Button and link faces
   `(link ((,class (:foreground ,fg :underline t))))
   `(link-visited ((,class (:foreground ,fg :underline t))))
   ;; `(button ((,class (:background ,hl :foreground ,fg))))
   `(header-line ((,class (:background ,bg :foreground ,fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'en4)

(provide 'en4-theme)
