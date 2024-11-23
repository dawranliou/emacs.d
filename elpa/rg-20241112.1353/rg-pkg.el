;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "rg" "20241112.1353"
  "A search tool based on ripgrep."
  '((emacs     "26.1")
    (transient "0.3.0")
    (wgrep     "2.1.10"))
  :url "https://github.com/dajva/rg.el"
  :commit "bd63cd3346fcabab0ff25e841e9d1ee56f5db705"
  :revdesc "bd63cd3346fc"
  :keywords '("matching" "tools")
  :authors '(("David Landell" . "david.landell@sunnyhill.email")
             ("Roland McGrath" . "roland@gnu.org"))
  :maintainers '(("David Landell" . "david.landell@sunnyhill.email")
                 ("Roland McGrath" . "roland@gnu.org")))
