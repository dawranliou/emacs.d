;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250605.1750"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "0.6.3")
    (magit-section "4.3.6")
    (seq           "2.24")
    (transient     "0.9.0")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "2e013df5ed069b0dbee6c12b1dc15af7544a4a03"
  :revdesc "2e013df5ed06"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
