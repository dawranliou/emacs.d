;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "markdown-toc" "20241226.1737"
  "A simple TOC generator for markdown file."
  '((emacs         "27.1")
    (markdown-mode "2.1")
    (dash          "2.11.0")
    (s             "1.9.0"))
  :url "http://github.com/ardumont/markdown-toc"
  :commit "d2fb4cbd95e558042307d706f9f47f93687c9fcc"
  :revdesc "d2fb4cbd95e5"
  :keywords '("markdown" "toc" "tools")
  :authors '(("Antoine R. Dumont" . "(@ardumont)"))
  :maintainers '(("Antoine R. Dumont" . "(@ardumont)")
                 ("Jen-Chieh Shen" . "jcs090218@gmail.com")))
