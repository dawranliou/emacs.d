;;; copilot-chat --- copilot-chat-shell-maker.el --- copilot chat interface, shell-maker frontend -*- lexical-binding: t; -*-

;; Copyright (C) 2024  copilot-chat maintainers

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


;;; Code:

(require 'shell-maker)

(require 'copilot-chat-copilot)
(require 'copilot-chat-markdown)

;; Constants
(defconst copilot-chat--shell-maker-temp-buffer-prefix
  "*copilot-chat-shell-maker-temp "
  "Temporary buffer prefix for Copilot Chat shell-maker.")

;; Functions
(defun copilot-chat--shell-maker-prompt-send()
  "Function to send the prompt content."
  (let ((instance (copilot-chat--current-instance)))
    (with-current-buffer (copilot-chat--shell-maker-get-buffer instance)
      (shell-maker-submit)
      (display-buffer (current-buffer)))))

(defun copilot-chat--shell-maker-temp-buffer-name (instance)
  "Return the temporary buffer name for the Copilot Chat shell-maker.
INSTANCE is used to get directory"
  (concat copilot-chat--shell-maker-temp-buffer-prefix
          (copilot-chat-directory instance)
          "*"))


(defun copilot-chat--shell-maker-get-buffer (instance)
  "Create or retrieve the Copilot Chat shell-maker buffer for INSTANCE."
  (unless (buffer-live-p (copilot-chat-chat-buffer instance))
    (setf (copilot-chat-chat-buffer instance)
          (copilot-chat--shell instance)))
  (let ((tempb (or (copilot-chat-shell-maker-tmp-buf instance)
                   (get-buffer-create
                    (copilot-chat--shell-maker-temp-buffer-name instance)))))
    (setf (copilot-chat-shell-maker-tmp-buf instance) tempb)
    (with-current-buffer tempb
      (let ((inhibit-read-only t))
        (markdown-view-mode)))
    (copilot-chat-chat-buffer instance)))

(defun copilot-chat--shell-maker-font-lock-faces (instance)
  "Replace faces by font-lock-faces in INSTANCE buffer."
  (with-current-buffer (copilot-chat-shell-maker-tmp-buf instance)
    (let ((inhibit-read-only t))
      (font-lock-ensure)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next-change (or (next-property-change (point)
                                                     nil
                                                     (point-max))
                               (point-max)))
              (face (get-text-property (point) 'face)))
          (when face
            (font-lock-append-text-property (point) next-change 'font-lock-face face))
          (goto-char next-change))))))

(defun copilot-chat--shell-maker-copy-faces(instance)
  "Apply faces to the copilot chat buffer corresponding to INSTANCE."
  (with-current-buffer (copilot-chat-shell-maker-tmp-buf instance)
    (save-restriction
      (widen)
      (font-lock-ensure)
      (copilot-chat--shell-maker-font-lock-faces instance)
      (let ((content (buffer-substring (point-min) (point-max))))
        (with-current-buffer (copilot-chat--shell-maker-get-buffer instance)
          (goto-char (1+ (copilot-chat-shell-maker-answer-point instance)))
          (insert content)
          (delete-region (point) (+ (point) (1- (length content))))
          (goto-char (point-max)))))))

(defun copilot-chat--shell-cb-prompt (instance shell content)
  "Callback for Copilot Chat `shell-maker'.
Argument INSTANCE is `copilot-chat' instance.
Argument SHELL is the `shell-maker' instance.
Argument CONTENT is copilot chat answer."
  (with-current-buffer (copilot-chat--shell-maker-get-buffer instance)
    (goto-char (point-max))
    (when (copilot-chat-first-word-answer instance)
      (setf (copilot-chat-first-word-answer instance) nil)
      (let ((str (concat (format-time-string "# [%T] ")
                         (format "Copilot(%s):\n"
                                 (copilot-chat-model instance))))
            (inhibit-read-only t))
        (with-current-buffer (copilot-chat-shell-maker-tmp-buf instance)
          (insert str))
        (funcall (map-elt shell :write-output) str)))
    (if (string= content copilot-chat--magic)
        (progn
          (funcall (map-elt shell :finish-output) t); the end
          (copilot-chat--shell-maker-copy-faces instance)
          (setf (copilot-chat-first-word-answer instance) t))
      (progn
        (with-current-buffer (copilot-chat-shell-maker-tmp-buf instance)
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert content)))
        (funcall (map-elt shell :write-output) content)))))

(defun copilot-chat--shell-cb-prompt-wrapper (shell instance content)
  "Wrapper around `copilot-chat--shell-cb-prompt'.
Argument SHELL is the `shell-maker' instance.
Argument INSTANCE is `copilot-chat' instance.
Argument CONTENT is copilot chat answer."
  (if copilot-chat-follow
      (copilot-chat--shell-cb-prompt instance shell content)
    (save-excursion
      (copilot-chat--shell-cb-prompt instance shell content))))

(defun copilot-chat--shell-cb (instance command shell)
  "Callback for Copilot Chat `shell-maker'.
Argument INSTANCE is `copilot-chat' instance.
Argument COMMAND is the command to send to Copilot.
Argument SHELL is the `shell-maker' instance."
  (setf
   (copilot-chat-shell-cb-fn instance) (apply-partially
                                        #'copilot-chat--shell-cb-prompt-wrapper
                                        shell)
   (copilot-chat-shell-maker-answer-point instance) (point))
  (let ((inhibit-read-only t))
    (with-current-buffer (copilot-chat-shell-maker-tmp-buf instance)
      (erase-buffer)))
  (copilot-chat--ask instance command (copilot-chat-shell-cb-fn instance)))

(defun copilot-chat--shell (instance)
  "Start a Copilot Chat shell for INSTANCE."
  (let ((buf (shell-maker-start
              (make-shell-maker-config
               :name (format "Copilot-Chat%s" (copilot-chat-directory instance))
               :execute-command (lambda (command shell)
                                  (copilot-chat--shell-cb instance command shell)))
              t nil t
              (copilot-chat--get-buffer-name (copilot-chat-directory instance)))))
    (with-current-buffer buf
      (setq-local default-directory (copilot-chat-directory instance)))
    buf))

(defun copilot-chat--shell-maker-insert-prompt(instance prompt)
  "Insert PROMPT in the chat buffer corresponding to INSTANCE."
  (with-current-buffer (copilot-chat--shell-maker-get-buffer instance)
    (goto-char (point-max))
    (insert prompt)))

(defun copilot-chat--shell-maker-clean()
  "Clean the copilot chat `shell-maker' frontend."
  (advice-remove 'copilot-chat-prompt-send #'copilot-chat--shell-maker-prompt-send))

(defun copilot-chat-shell-maker-init()
  "Initialize the copilot chat `shell-maker' frontend."
  (setq copilot-chat-prompt copilot-chat-markdown-prompt)
  (advice-add 'copilot-chat-prompt-send
              :override
              #'copilot-chat--shell-maker-prompt-send))

;; Top-level execute code.

(cl-pushnew
 (make-copilot-chat-frontend
  :id 'shell-maker
  :init-fn #'copilot-chat-shell-maker-init
  :clean-fn #'copilot-chat--shell-maker-clean
  :format-fn nil
  :format-code-fn #'copilot-chat--markdown-format-code
  :format-buffer-fn #'copilot-chat--markdown-format-buffer
  :create-req-fn nil
  :send-to-buffer-fn nil
  :copy-fn nil
  :yank-fn nil
  :write-fn nil
  :get-buffer-fn #'copilot-chat--shell-maker-get-buffer
  :insert-prompt-fn #'copilot-chat--shell-maker-insert-prompt
  :pop-prompt-fn nil
  :goto-input-fn #'nil
  :get-spinner-buffer-fn #'copilot-chat--shell-maker-get-buffer)
 copilot-chat--frontend-list
 :test #'equal)

(provide 'copilot-chat-shell-maker)
;;; copilot-chat-shell-maker.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
