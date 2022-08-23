(deftheme bit
  "A bit of color theme.

https://lospec.com/palette-list/bit-interior

- #f3e2b1
- #f7c439
- #ff880b
- #ff5036
- #ae3737
- #e05ad1
- #4dd464
- #5fb7f3
- #c98c4b
- #a26134
- #bec4bb
- #929992
- #2e2823
- #331b0b
- #201208

https://lospec.com/palette-list/bitbits-neon

- #222323
- #ff4adc
- #3dff98
- #f0f6f0
")

(let ((class '((class color) (min-colors 89)))
      (fg "#f0f6f0")
      (bg "#222323")
      ;; (bg "#f0f6f0")
      ;; (fg "#222323")
      (pop "#3dff98")
      (dim "#FCE2DB")
      (hl "#ff4adc"))
  (custom-theme-set-faces
   'bit
   `(default ((,class (:background ,bg :foreground ,fg
                                   :weight normal
                                   ))))
   `(cursor ((,class (:background ,pop))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,bg))))
   `(highlight ((,class (:background ,hl :foreground ,fg))))
   `(region ((,class (:background ,hl :foreground ,fg))))
   `(secondary-selection ((,class (:background ,hl :foreground ,fg))))
   `(isearch ((,class (:background ,hl :foreground ,fg))))
   `(lazy-highlight ((,class (:background ,dim :foreground ,bg))))
   `(show-paren-match ((,class (:background ,hl :bold t))))
   ;; Mode line faces
   `(mode-line ((,class (:inverse-video t :foreground ,pop))))
   `(mode-line-inactive ((,class (:inverse-video t))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,fg))))
   `(escape-glyph ((,class (:foreground ,fg :weight bold))))
   `(homoglyph ((,class (:foreground ,fg :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,fg))))
   `(font-lock-comment-face ((,class (:italic t :weight light :foreground ,hl))))
   `(font-lock-constant-face ((,class (:foreground ,fg :bold t))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :bold t))))
   `(font-lock-keyword-face ((,class (:foreground ,fg))))
   `(font-lock-string-face ((,class (:foreground ,hl :weight light))))
   `(font-lock-type-face ((,class (:foreground ,fg))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg :bold t))))
   ;; `(font-lock-warning-face ((,class (:weight bold))))
   ;; Button and link faces
   `(link ((,class (:foreground ,fg :underline t))))
   `(link-visited ((,class (:foreground ,fg :underline t))))
   ;; `(button ((,class (:background ,hl :foreground ,fg))))
   `(header-line ((,class (:background ,bg :foreground ,fg))))
   ))
