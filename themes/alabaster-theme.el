(deftheme alabaster
  "https://github.com/tonsky/sublime-scheme-alabaster")

;; https://github.com/tonsky/sublime-scheme-alabaster/blob/master/Alabaster%20BG.sublime-color-scheme
;; {
;;     "name": "Alabaster BG",
;;     "author": "Nikita Prokopov",
;;     "variables":
;;     {
;;         "active":     "#007ACC",
;;         "fg":         "#000",
;;         "bg":         "#fff",
;;         "blue":       "#DBF1FF",
;;         "green":      "#F1FADF",
;;         "dark_green": "#DBECB6",
;;         "red":        "#FFE0E0",
;;         "magenta":    "#F9E0FF",
;;         "yellow":     "#FFFABC",
;;         "orange":     "#FFBC5D",
;;     },
;;     "globals":
;;     {
;;         "foreground":                  "var(fg)",
;;         "background":                  "var(bg)",
;;         "caret":                       "var(active)",
;;         "line_highlight":              "#00000010",
;;         "misspelling":                 "#f00",
;;         "selection":                   "#B4D8FD",
;;         "selection_border_width":      "0",
;;         "inactive_selection":          "#E0E0E0",
;;         "selection_corner_radius":     "2",
;;         "highlight":                   "var(orange)",
;;         "find_highlight_foreground":   "#000",
;;         "find_highlight":              "var(orange)",
;;         "brackets_options":            "underline",
;;         "brackets_foreground":         "var(active)",
;;         "bracket_contents_options":    "underline",
;;         "bracket_contents_foreground": "var(active)",
;;         "tags_options":                "underline",
;;         "tags_foreground":             "var(active)",
;;     },
;;     "rules":
;;     [
;;         {"name":       "Comments",
;;          "scope":      "comment, punctuation.definition.comment, invalid comment",
;;          "foreground": "#000",
;;          "background": "var(yellow)"},
;;         {"name":       "Strings",
;;          "scope":      "string",
;;          "background": "var(green)"},
;;         {"name":       "Escapes",
;;          "scope":      "constant.character.escape, constant.other.placeholder",
;;          "background": "var(dark_green)"},
;;         // {"name":       "Constants",
;;         //  "scope":      "constant",
;;         //  "background": "var(magenta)"},
;;         {"name":       "Constants",
;;          "scope":      "constant, punctuation.definition.constant",
;;          "foreground": "#7A3E9D"},
;;         {"name":       "Definitions",
;;          "scope":      "entity.name - entity.name.tag",
;;          "background": "var(blue)"},
;;         {"name":       "Punctuation",
;;          "scope":      "punctuation - punctuation.section, keyword.operator, string punctuation.section",
;;          "foreground": "#00000090"},
;;         {"name":       "Inner brackets",
;;          "scope":      "meta.parens meta.parens punctuation.section, meta.parens meta.brackets punctuation.section, meta.parens meta.braces punctuation.section, meta.brackets meta.parens punctuation.section, meta.brackets meta.brackets punctuation.section, meta.brackets meta.braces punctuation.section, meta.braces meta.parens punctuation.section, meta.braces meta.brackets punctuation.section, meta.braces meta.braces punctuation.section",
;;          "foreground": "#00000075"},
;;         {"name":       "Mistakes",
;;          "scope":      "invalid, invalid string, invalid constant, invalid entity.name, invalid punctuation, invalid source.symbol",
;;          "foreground": "#c33",
;;          "background": "var(red)"},
;;         {"scope": "markup.inserted",
;;          "foreground": "hsl(100, 50%, 50%)"},
;;         {"scope": "markup.deleted",
;;          "foreground": "hsl(2, 65%, 50%)"},
;;         {"scope": "markup.changed",
;;          "foreground": "hsl(30, 85%, 50%)"},
;;         {"scope": "markup.ignored",
;;          "foreground": "#aaa"},
;;         {"scope": "markup.untracked",
;;          "foreground": "#aaa"}
;;     ]
;; }

(custom-theme-set-faces
 'alabaster
 '(cursor ((t (:background "#007ACC"))))
 ;; '(fixed-pitch ((t (:family "Monospace"))))
 ;; '(variable-pitch ((t (:family "Sans Serif"))))
 ;; '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 ;; '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 ;; '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 ;; '(highlight ((((class color) (min-colors 88) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 88) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 16) (background light)) (:background "darkseagreen2")) (((class color) (min-colors 16) (background dark)) (:background "darkolivegreen")) (((class color) (min-colors 8)) (:foreground "black" :background "green")) (t (:inverse-video t))))
 ;; '(region ((((class color) (min-colors 88) (background dark)) (:extend t :background "blue3")) (((class color) (min-colors 88) (background light)) (:extend t :background "lightgoldenrod2")) (((class color) (min-colors 16) (background dark)) (:extend t :background "blue3")) (((class color) (min-colors 16) (background light)) (:extend t :background "lightgoldenrod2")) (((class color) (min-colors 8)) (:extend t :foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:extend t :background "gray"))))
 ;; '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 ;; '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 ;; '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-bracket-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-delimiter-face ((t (:background "#FFFABC" :extend t))))
 '(font-lock-comment-face ((t (:background "#FFFABC" :extend t))))
 '(font-lock-constant-face ((t (:foreground "#7A3E9D"))))
 '(font-lock-delimiter-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-doc-face ((t (:background "#FFFABC"))))
 '(font-lock-doc-markup-face ((t (:background "#FFFABC"))))
 '(font-lock-escape-face ((t (:inherit (font-lock-regexp-grouping-backslash)))))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-number-face ((t nil)))
 '(font-lock-misc-punctuation-face ((t (:inherit (font-lock-punctuation-face)))))
 '(font-lock-operator-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-property-face ((t (:background "#DBF1FF"))))
 '(font-lock-punctuation-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:background "#F1FADF"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t (:bold t))))
 '(font-lock-warning-face ((t (:background "#FFE0E0" :foreground "#C33"))))
 ;; '(button ((t (:inherit (link)))))
 ;; '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line :position nil) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line :position nil) :foreground "cyan")) (t (:inherit (underline)))))
 ;; '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 ;; '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
 ;; '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line :position nil) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line :position nil) :box nil :inverse-video nil :foreground "white" :background "black"))))
 ;; '(tooltip ((t (:foreground "black" :background "lightyellow"))))
 ;; '(mode-line ((((class color) (min-colors 88)) (:foreground "black" :background "grey75" :box (:line-width (1 . -1) :color nil :style released-button))) (t (:inverse-video t))))
 ;; '(mode-line-buffer-id ((t (:weight bold))))
 ;; '(mode-line-emphasis ((t (:weight bold))))
 ;; '(mode-line-highlight ((((supports :box t) (class color) (min-colors 88)) (:box (:line-width (2 . 2) :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 ;; '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "grey90" :foreground "grey20" :box (:line-width (1 . -1) :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width (1 . -1) :color "grey40" :style nil) :weight light))))
 ;; '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 ;; '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:background "#DBF1FF"))))
 ;; '(match ((((class color) (min-colors 88) (background light)) (:background "khaki1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 ;; '(next-error ((t (:inherit (region)))))
 ;; '(query-replace ((t (:inherit (isearch)))))
 ;; '(default ((t (:family "Iosevka" :foundry "nil" :width normal :height 150 :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "black" :background "white" :stipple nil :inherit nil))))
 ;; '(clojure-keyword-face ((t (:inherit (font-lock-constant-face)))))
 ;; '(show-paren-match ((t (:background "#DBECB6"))))
 ;; '(show-paren-match-expression ((t (:background "#DBECB6"))))
 )

(provide-theme 'alabaster)
