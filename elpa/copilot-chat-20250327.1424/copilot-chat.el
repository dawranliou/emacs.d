;;; copilot-chat.el --- Copilot chat interface -*- lexical-binding: t -*-

;; Copyright (C) 2024  copilot-chat maintainers

;; Author: cedric.chepied <cedric.chepied@gmail.com>
;; Package-Version: 20250327.1424
;; Package-Revision: fb789c721e06
;; URL: https://github.com/chep/copilot-chat.el
;; Package-Requires: ((emacs "27.1") (aio "1.0") (request "0.3.2") (transient "0.8.3") (polymode "0.2.2") (org "9.4.6") (markdown-mode "2.6") (shell-maker "0.76.2"))
;; Keywords: convenience, tools


;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Add the ability to chat with github copilot

;;; Code:

(require 'copilot-chat-command)
(require 'copilot-chat-common)
(require 'copilot-chat-copilot)
(require 'copilot-chat-frontend)
(require 'copilot-chat-prompts)
(require 'copilot-chat-transient)

(defcustom copilot-chat-frontend 'org
  "Frontend to use with `copilot-chat'.  Can be org or markdown."
  :type '(choice (const :tag "org-mode" org)
                 (const :tag "markdown" markdown)
                 (const :tag "shell-maker" shell-maker))
  :set (lambda (symbol value)
         (set-default-toplevel-value symbol value)
         (pcase value
           (`org (require 'copilot-chat-org))
           (`markdown (require 'copilot-chat-markdown))
           (`shell-maker (require 'copilot-chat-shell-maker))))
  :group 'copilot-chat)

(provide 'copilot-chat)
;;; copilot-chat.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; End:
