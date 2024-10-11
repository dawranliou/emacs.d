;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20241002.2036"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.3.0")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "d727fe8466502e29975067adf6f2ca3e0618279c"
  :revdesc "d727fe846650"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
