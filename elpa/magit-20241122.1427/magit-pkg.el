;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20241122.1427"
  "A Git porcelain inside Emacs."
  '((emacs         "26.1")
    (compat        "30.0.0.0")
    (dash          "2.19.1")
    (magit-section "4.1.2")
    (seq           "2.24")
    (transient     "0.7.8")
    (with-editor   "3.4.2"))
  :url "https://github.com/magit/magit"
  :commit "3deaad3d29a552e6cf9370dcb0b6a0e676ffb5bc"
  :revdesc "3deaad3d29a5"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
