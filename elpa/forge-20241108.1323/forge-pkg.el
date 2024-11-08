;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20241108.1323"
  "Access Git forges from Magit."
  '((emacs         "27.1")
    (compat        "30.0.0.0")
    (closql        "2.0.0")
    (dash          "2.19.1")
    (emacsql       "4.0.3")
    (ghub          "4.1.1")
    (let-alist     "1.0.6")
    (magit         "4.1.1")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.7.6")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "9ac2afbbbc13e32cad878bb33f69e1fc36d9f94e"
  :revdesc "9ac2afbbbc13"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
