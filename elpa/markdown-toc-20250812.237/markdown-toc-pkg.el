;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "markdown-toc" "20250812.237"
  "A simple TOC generator for markdown file."
  '((emacs         "28.1")
    (markdown-mode "2.1")
    (dash          "2.11.0")
    (s             "1.9.0"))
  :url "http://github.com/ardumont/markdown-toc"
  :commit "3835390ac44c5dc712d4caae5661995f0639262e"
  :revdesc "3835390ac44c"
  :keywords '("markdown" "toc" "tools")
  :authors '(("Antoine R. Dumont" . "(@ardumont)"))
  :maintainers '(("Antoine R. Dumont" . "(@ardumont)")
                 ("Jen-Chieh Shen" . "jcs090218@gmail.com")))
