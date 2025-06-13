;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250610.1559"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "0.6.3")
    (magit-section "4.3.6")
    (seq           "2.24")
    (transient     "0.9.0")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "31afaddc07b2a9808178d4c59d8c896146d4dfad"
  :revdesc "31afaddc07b2"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
