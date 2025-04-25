;;; copilot-chat --- copilot-chat-command.el --- copilot chat command -*- lexical-binding: t; -*-

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

(require 'polymode)

(require 'copilot-chat-copilot)
(require 'copilot-chat-git)
(require 'copilot-chat-prompt-mode)

;; customs
(defcustom copilot-chat-list-added-buffers-only nil
  "If non-nil, only show buffers that have been added to the Copilot chat list."
  :type 'boolean
  :group 'copilot-chat)

;; Faces
(defface copilot-chat-list-selected-buffer-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected buffers in `copilot-chat' buffer list."
  :group 'copilot-chat)
(defface copilot-chat-list-default-face
  '((t :inherit default))
  "Face used for unselected buffers in `copilot-chat' buffer list."
  :group 'copilot-chat)

;; Variables
(defvar copilot-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "SPC") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "C-c C-c") 'copilot-chat-list-clear-buffers)
    (define-key map (kbd "g") (lambda()
                                (interactive)
                                (copilot-chat-list-refresh (copilot-chat--current-instance))))
    (define-key map (kbd "q") (lambda()
                                (interactive)
                                (bury-buffer)
                                (delete-window)))
    map)
  "Keymap for `copilot-chat-list-mode'.")

;; Functions
(define-derived-mode copilot-chat-list-mode special-mode "Copilot Chat List"
  "Major mode for listing and managing buffers in Copilot chat."
  (setq buffer-read-only t))

(defun copilot-chat-prompt-send ()
  "Send the prompt content to Copilot.
Retrieves the current prompt, displays it in the chat buffer, and sends it
to Copilot for processing."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (when instance
      (copilot-chat--display instance)
      (let ((prompt (copilot-chat--pop-current-prompt instance)))
        (copilot-chat--write-buffer
         instance
         (copilot-chat--format-data instance prompt 'prompt) nil)
        (with-current-buffer (copilot-chat--get-buffer instance)
          (recenter-top-bottom))
        (push prompt (copilot-chat-prompt-history instance))
        (setf (copilot-chat-prompt-history-position instance) nil)
        (copilot-chat--ask instance prompt 'copilot-chat-prompt-cb)))))

;;;###autoload (autoload 'copilot-chat-ask-and-insert "copilot-chat" nil t)
(defun copilot-chat-ask-and-insert ()
  "Send to Copilot a custom prompt and insert answer in current buffer at point."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (prompt (read-from-minibuffer "Copilot prompt: "))
         (current-buf (current-buffer)))
    (copilot-chat--ask
     instance
     prompt
     (lambda (instance content)
       (copilot-chat-prompt-cb instance content current-buf)))))

(defun copilot-chat--ask-region(prompt beg end)
  "Send to Copilot a prompt followed by the current selected code.
Argument PROMPT is the prompt to send to Copilot.
BEG and END are the beginning and end positions of the region to explain."
  (let ((instance (copilot-chat--current-instance))
        (code (buffer-substring-no-properties beg end)))
    (copilot-chat--insert-and-send-prompt
     instance
     (concat (cdr (assoc prompt (copilot-chat--prompts)))
             (copilot-chat--format-code code)))))

;;;###autoload (autoload 'copilot-chat-explain "copilot-chat" nil t)
(defun copilot-chat-explain (beg end)
  "Ask Copilot to explain the current selected code.
BEG and END are the beginning and end positions of the region to explain."
  (interactive "r")
  (copilot-chat--ask-region 'explain beg end))

;;;###autoload (autoload 'copilot-chat-review "copilot-chat" nil t)
(defun copilot-chat-review (beg end)
  "Ask Copilot to review the current selected code.
BEG and END are the beginning and end positions of the region to review."
  (interactive "r")
  (copilot-chat--ask-region 'review beg end))

;;;###autoload (autoload 'copilot-chat-doc "copilot-chat" nil t)
(defun copilot-chat-doc (beg end)
  "Ask Copilot to write documentation for the current selected code.
BEG and END are the beginning and end positions of the region to document."
  (interactive "r")
  (copilot-chat--ask-region 'doc beg end))

;;;###autoload (autoload 'copilot-chat-fix "copilot-chat" nil t)
(defun copilot-chat-fix (beg end)
  "Ask Copilot to fix the current selected code.
BEG and END are the beginning and end positions of the region to fix."
  (interactive "r")
  (copilot-chat--ask-region 'fix beg end))

;;;###autoload (autoload 'copilot-chat-optimize "copilot-chat" nil t)
(defun copilot-chat-optimize (beg end)
  "Ask Copilot to optimize the current selected code.
BEG and END are the beginning and end positions of the region to optimize."
  (interactive "r")
  (copilot-chat--ask-region 'optimize beg end))

;;;###autoload (autoload 'copilot-chat-test "copilot-chat" nil t)
(defun copilot-chat-test (beg end)
  "Ask Copilot to generate test for the current selected code.
BEG and END are the beginning and end positions of the region to test."
  (interactive "r")
  (copilot-chat--ask-region 'test beg end))

(defun copilot-chat--insert-prompt (instance prompt)
  "Insert PROMPT in the Copilot Chat prompt region.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to insert in the prompt region."
  (let ((prompt-fn (copilot-chat-frontend-insert-prompt-fn
                    (copilot-chat--get-frontend))))
    (when prompt-fn
      (funcall prompt-fn instance prompt))))

(defun copilot-chat--insert-and-send-prompt (instance prompt)
  "Helper function to prepare buffer and send PROMPT to Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to send to Copilot."
  (copilot-chat--insert-prompt instance prompt)
  (copilot-chat-prompt-send))

(defun copilot-chat--get-language ()
  "Get the current language of the buffer.
Derives language name from the major mode of the current buffer."
  (if (derived-mode-p 'prog-mode)  ; current buffer is a programming language buffer
      (let* ((major-mode-str (symbol-name major-mode))
             (lang (replace-regexp-in-string "\\(?:-ts\\)?-mode$" "" major-mode-str)))
        lang)
    nil))

(defun copilot-chat--format-code(code)
  "Format code according to the frontend.
Argument CODE is the code to be formatted."
  (let ((format-fn (copilot-chat-frontend-format-code-fn (copilot-chat--get-frontend))))
    (if format-fn
        (funcall format-fn code (copilot-chat--get-language))
      code)))

;;;###autoload (autoload 'copilot-chat-explain-symbol-at-line "copilot-chat" nil t)
(defun copilot-chat-explain-symbol-at-line ()
  "Ask Copilot to explain symbol under point.
Given the code line as background info."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prompt (format "Please explain what '%s' means in the context of this code line:\n%s"
                         symbol (copilot-chat--format-code line))))
    (copilot-chat--insert-and-send-prompt instance prompt)))

;;;###autoload (autoload 'copilot-chat-explain-defun "copilot-chat" nil t)
(defun copilot-chat-explain-defun ()
  "Mark current function definition and ask Copilot to explain it, then unmark."
  (interactive)
  (save-excursion
    (mark-defun)
    (call-interactively 'copilot-chat-explain)
    (deactivate-mark)))

;;;###autoload (autoload 'copilot-chat-custom-prompt-function "copilot-chat" nil t)
(defun copilot-chat-custom-prompt-function ()
  "Mark current function and ask `copilot-chat' with custom prompt."
  (interactive)
  (save-excursion
    (mark-defun)
    (copilot-chat-custom-prompt-selection)
    (deactivate-mark)))

;;;###autoload (autoload 'copilot-chat-review-whole-buffer "copilot-chat" nil t)
(defun copilot-chat-review-whole-buffer ()
  "Mark whole buffer, ask Copilot to review it, then unmark.
It can be used to review the magit diff for my change, or other people's"
  (interactive)
  (save-excursion
    (copilot-chat-review (point-min) (point-max))))

;;;###autoload (autoload 'copilot-chat-switch-to-buffer "copilot-chat" nil t)
(defun copilot-chat-switch-to-buffer ()
  "Switch to Copilot Chat buffer.
Side by side with the current code editing buffer."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (unless (equal (pm-base-buffer) (copilot-chat--get-buffer instance))
      (switch-to-buffer-other-window (copilot-chat--get-buffer instance))))
  (copilot-chat-goto-input))

;;;###autoload (autoload 'copilot-chat-custom-prompt-selection "copilot-chat" nil t)
(defun copilot-chat-custom-prompt-selection (&optional custom-prompt)
  "Send to Copilot a custom prompt followed by the current selected code/buffer.
If CUSTOM-PROMPT is provided, use it instead of reading from the mini-buffer."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (prompt (or custom-prompt (read-from-minibuffer "Copilot prompt: ")))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (formatted-prompt (concat prompt "\n" (copilot-chat--format-code code))))
    (copilot-chat--insert-and-send-prompt instance formatted-prompt)))

;;;###autoload (autoload 'copilot-chat-custom-prompt-mini-buffer "copilot-chat" nil t)
(defun copilot-chat-custom-prompt-mini-buffer ()
  "Read a string with Helm completion, showing historical inputs."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (prompt "Question for copilot-chat: ")
         (input (read-string prompt nil 'copilot-chat--prompt-history)))
    (copilot-chat--insert-and-send-prompt instance input)))

;;;###autoload (autoload 'copilot-chat-list "copilot-chat" nil t)
(defun copilot-chat-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (buffer (copilot-chat--get-list-buffer-create instance)))
    (with-current-buffer buffer
      (copilot-chat-list-mode))
    (copilot-chat-list-refresh instance)
    (switch-to-buffer buffer)))

(defun copilot-chat--display (instance)
  "Internal function to display copilot chat buffer.
Argument INSTANCE is the copilot chat instance to display."
  (let ((base-buffer (copilot-chat--get-buffer instance))
        (init-fn (copilot-chat-frontend-init-fn
                  (copilot-chat--get-frontend)))
        (window-found nil))
    (when init-fn
      (funcall init-fn))
    ;; Check if any window is already displaying the base buffer or an indirect
    ;; buffer
    (cl-block window-search
      (dolist (window (window-list))
        (let ((buf (window-buffer window)))
          (when (or (eq buf base-buffer)
                    (eq (with-current-buffer buf (pm-base-buffer)) base-buffer))
            (select-window window)
            (switch-to-buffer base-buffer)
            (setq window-found t)
            (cl-return-from window-search)))))

    (unless window-found
      (pop-to-buffer base-buffer))))

;;;###autoload (autoload 'copilot-chat-display "copilot-chat" nil t)
(defun copilot-chat-display (&optional arg)
  "Display copilot chat buffer.
With prefix argument, explicitly ask for which instance to use.
Optional argument ARG if non-nil, force instance selection."
  (interactive "P")
  (let ((instance (if arg
                      (copilot-chat--ask-for-instance)
                    (copilot-chat--current-instance))))
    (copilot-chat--display instance)))

;;;###autoload (autoload 'copilot-chat "copilot-chat" nil t)
(defalias 'copilot-chat 'copilot-chat-display)

;;;###autoload (autoload 'copilot-chat-hide "copilot-chat" nil t)
(defun copilot-chat-hide ()
  "Hide copilot chat buffer."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (base-buffer (copilot-chat--get-buffer instance)))
    (dolist (window (window-list))
      (let ((buf (window-buffer window)))
        (when (or (eq buf base-buffer)
                  (eq (with-current-buffer buf (pm-base-buffer)) base-buffer))
          (delete-window window))))))

(defun copilot-chat-add-current-buffer ()
  "Add current buffer in sent buffers list."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (copilot-chat--add-buffer instance (current-buffer))
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-del-current-buffer ()
  "Remove current buffer from sent buffers list."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (copilot-chat--del-buffer instance (current-buffer))
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-add-buffers (buffers)
  "Add BUFFERS to sent buffers list."
  (interactive
   (list (completing-read-multiple "Buffers: "
                                   (mapcar #'buffer-name (buffer-list))
                                   nil t (buffer-name (current-buffer)))))
  (let ((instance (copilot-chat--current-instance)))
    (mapc (lambda (buf)
            (copilot-chat--add-buffer instance buf))
          buffers)
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-del-buffers (buffers)
  "Remove BUFFERS from sent buffers list."
  (interactive
   (list
    (completing-read-multiple "Buffers: "
                              (mapcar #'buffer-name (buffer-list))
                              nil t (buffer-name (current-buffer)))))
  (let ((instance (copilot-chat--current-instance)))
    (mapc (lambda (buf)
            (copilot-chat--del-buffer instance buf))
          buffers)
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-add-file (file-path)
  "Add FILE-PATH to `copilot-chat' buffers without changing current window layout."
  (interactive "fFile to add: ")
  (save-window-excursion
    (let ((current-buf (current-buffer)))
      (find-file file-path)
      (copilot-chat-add-current-buffer)
      (switch-to-buffer current-buf))))

(defun copilot-chat-add-buffers-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Copilot chat."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list)))
        (added-buffers '()))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when buffer-file-name
          (copilot-chat-add-current-buffer)
          (push (buffer-name buffer) added-buffers))))
    (message "Added buffers: %s" (string-join added-buffers ", "))))

(defun copilot-chat-add-files-under-dir ()
  "Add all files with same suffix as current file under current directory.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (dolist (file files)
          (copilot-chat-add-file file))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

(defun copilot-chat--buffer-list (instance)
  "Return a list of buffer with files in INSTANCE directory."
  (let* ((dir (copilot-chat-directory instance))
         (bufs-under-dir (cl-remove-if-not
                          (lambda (buf)
                            (with-current-buffer buf
                              (and buffer-file-name
                                   (file-in-directory-p buffer-file-name dir))))
                          (buffer-list))))
    (cl-union bufs-under-dir (copilot-chat-buffers instance) :test #'eq)))

(defun copilot-chat-list-refresh (&optional instance)
  "Refresh the list of buffers in the current Copilot chat list buffer.
Optional argument INSTANCE specifies which instance to refresh the list for."
  (interactive)
  (unless instance
    (setq instance (copilot-chat--current-instance)))
  (setf (copilot-chat-buffers instance)
        (cl-remove-if-not #'buffer-live-p (copilot-chat-buffers instance)))
  (with-current-buffer (copilot-chat--get-list-buffer-create instance)
    (let* ((pt (point))
           (inhibit-read-only t)
           (buffers (if copilot-chat-list-added-buffers-only
                        (copilot-chat-buffers instance)
                      (copilot-chat--buffer-list instance)))
           (sorted-buffers
            (sort buffers
                  (lambda (a b)
                    (string< (symbol-name (buffer-local-value 'major-mode a))
                             (symbol-name (buffer-local-value 'major-mode b)))))))
      (erase-buffer)
      (dolist (buffer sorted-buffers)
        (let ((buffer-name (buffer-name buffer))
              (cop-bufs (copilot-chat--get-buffers instance)))
          (when (and (not (string-prefix-p " " buffer-name))
                     (not (string-prefix-p "*" buffer-name)))
            (insert (propertize buffer-name
                                'face (if (member buffer cop-bufs)
                                          'copilot-chat-list-selected-buffer-face
                                        'copilot-chat-list-default-face))
                    "\n"))))
      (goto-char pt))))


(defun copilot-chat-list-add-or-remove-buffer ()
  "Add or remove the buffer at point from the Copilot chat list."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (buffer-name (buffer-substring (line-beginning-position)
                                        (line-end-position)))
         (buffer (get-buffer buffer-name))
         (cop-bufs (copilot-chat--get-buffers instance)))
    (when buffer
      (if (member buffer cop-bufs)
          (progn
            (copilot-chat--del-buffer instance buffer)
            (message "Buffer '%s' removed from Copilot chat list." buffer-name))
        (copilot-chat--add-buffer instance buffer)
        (message "Buffer '%s' added to Copilot chat list." buffer-name)))
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-list-clear-buffers ()
  "Clear all buffers from the Copilot chat list."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (copilot-chat--clear-buffers instance)
    (message "Cleared all buffers from Copilot chat list.")
    (copilot-chat-list-refresh instance)))

(defun copilot-chat-prompt-split-and-list()
  "Split prompt window and display buffer list."
  (interactive)
  (let ((split-window-preferred-function nil)
        (split-height-threshold nil)
        (split-width-threshold nil))
    (split-window-right (floor (* 0.8 (window-total-width)))))
  (other-window 1)
  (copilot-chat-list))

(defun copilot-chat-prompt-history-previous()
  "Insert previous prompt in prompt buffer."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (prompt (if (null (copilot-chat-prompt-history instance))
                     nil
                   (if (null (copilot-chat-prompt-history-position instance))
                       (progn
                         (setf (copilot-chat-prompt-history-position instance) 0)
                         (car (copilot-chat-prompt-history instance)))
                     (if (= (copilot-chat-prompt-history-position instance)
                            (1- (length (copilot-chat-prompt-history instance))))
                         (car (last (copilot-chat-prompt-history instance)))
                       (setf (copilot-chat-prompt-history-position instance)
                             (1+ (copilot-chat-prompt-history-position instance)))
                       (nth (copilot-chat-prompt-history-position instance)
                            (copilot-chat-prompt-history instance)))))))
    (when prompt
      (copilot-chat--insert-prompt instance prompt))))


(defun copilot-chat-prompt-history-next()
  "Insert next prompt in prompt buffer."
  (interactive)
  (let* ((instance (copilot-chat--current-instance))
         (prompt (if (null (copilot-chat-prompt-history instance))
                     nil
                   (if (null (copilot-chat-prompt-history-position instance))
                       nil
                     (if (= 0 (copilot-chat-prompt-history-position instance))
                         ""
                       (progn
                         (setf (copilot-chat-prompt-history-position instance)
                               (1- (copilot-chat-prompt-history-position instance)))
                         (nth (copilot-chat-prompt-history-position instance)
                              (copilot-chat-prompt-history instance))))))))
    (when prompt
      (copilot-chat--insert-prompt instance prompt))))

(defun copilot-chat-reset (&optional keep-buffers)
  "Reset copilot chat session.
When called interactively with prefix argument, preserve the buffer list.
Optional argument KEEP-BUFFERS if non-nil, preserve the current buffer list."
  (interactive "P")
  (let* ((instance (copilot-chat--current-instance))
         (old-buffers (when keep-buffers
                        (copilot-chat-buffers instance)))
         (buf (copilot-chat--get-buffer instance)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf (copilot-chat-history instance) nil
          (copilot-chat-prompt-history instance) nil
          (copilot-chat-prompt-history-position instance) nil
          (copilot-chat-yank-index instance) 1
          (copilot-chat-last-yank-start instance) nil
          (copilot-chat-last-yank-end instance) nil
          (copilot-chat-spinner-timer instance) nil
          (copilot-chat-spinner-index instance) 0
          (copilot-chat-spinner-status instance) nil)
    (unless old-buffers
      (setf (copilot-chat-buffers instance) nil))
    (copilot-chat--display instance)
    (copilot-chat-list-refresh instance)))

(defun copilot-chat--clean()
  "Cleaning function."
  (let ((clean-fn (copilot-chat-frontend-clean-fn (copilot-chat--get-frontend))))
    (when clean-fn
      (funcall clean-fn))))


(defun copilot-chat-send-to-buffer()
  "Send the code block at point to buffer.
Replace selection if any."
  (interactive)
  (let ((send-fn (copilot-chat-frontend-send-to-buffer-fn
                  (copilot-chat--get-frontend))))
    (when send-fn
      (funcall send-fn))))

(defun copilot-chat-copy-code-at-point ()
  "Copy the code block at point into kill ring."
  (interactive)
  (let ((copy-fn (copilot-chat-frontend-copy-fn (copilot-chat--get-frontend))))
    (when copy-fn
      (funcall copy-fn))))

(defun copilot-chat-goto-input()
  "Go to the input area."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (when (equal (pm-base-buffer) (copilot-chat--get-buffer instance))
      (let ((goto-fn (copilot-chat-frontend-goto-input-fn (copilot-chat--get-frontend))))
        (when goto-fn
          (funcall goto-fn))))))

(defun copilot-chat--model-picker-enabled (model)
  "Check the `model_picker_enabled` attribute of the MODEL.
For example, GPT-3.5 has no more significance
for most people nowadays than GPT-4o."
  (eq t (alist-get 'model_picker_enabled model)))

(defun copilot-chat--model-enabled-p (model)
  "Return non-nil if MODEL is enabled.
The model is enabled if it has no policy or if its policy state is \"enabled\".
This function checks the JSON policy data returned from the API."
  (let ((policy (alist-get 'policy model)))
    (or (not policy)
        (equal (alist-get 'state policy) "enabled"))))

(defun copilot-chat--get-model-choices-with-wait ()
  "Get the list of available models for Copilot Chat.
waiting for fetch if needed.
If models haven't been fetched yet and no cache exists,
wait for the fetch to complete."
  (let ((models
         (seq-filter #'copilot-chat--model-enabled-p
           (if copilot-chat-model-ignore-picker
               (copilot-chat-connection-models copilot-chat--connection)
             (seq-filter #'copilot-chat--model-picker-enabled
                         (copilot-chat-connection-models
                          copilot-chat--connection))))))
    (if models
        (let* ((model-info-list
                (mapcar
                 (lambda (model)
                   (let* ((id (alist-get 'id model))
                          (name (alist-get 'name model))
                          (clean-name (replace-regexp-in-string " (Preview)$" "" name))
                          (vendor (alist-get 'vendor model))
                          (capabilities (alist-get 'capabilities model))
                          (limits (alist-get 'limits capabilities))
                          (preview-p (eq t (alist-get 'preview model)))
                          (preview-text (if preview-p " (Preview)" ""))
                          (prompt-tokens (alist-get 'max_prompt_tokens limits))
                          (output-tokens (or (alist-get 'max_output_tokens limits)
                                             (when (string-prefix-p "o1" id) 100000)))
                          (prompt-text (if prompt-tokens
                                           (format "%dk" (round (/ prompt-tokens 1000)))
                                         "?"))
                          (output-text (if output-tokens
                                           (format "%dk" (round (/ output-tokens 1000)))
                                         "?")))
                     (list :id id
                           :vendor vendor
                           :clean-name clean-name
                           :preview-text preview-text
                           :prompt-text prompt-text
                           :output-text output-text)))
                 models))
               (max-vendor-width
                (apply #'max
                       (mapcar (lambda (info)
                                 (length (plist-get info :vendor))) model-info-list)))
               (max-name-width
                (apply #'max
                       (mapcar (lambda (info)
                                 (length (plist-get info :clean-name))) model-info-list)))
               (max-preview-width
                (apply #'max
                       (mapcar (lambda (info)
                                 (length (plist-get info :preview-text))) model-info-list)))
               (max-prompt-width
                (apply #'max
                       (mapcar (lambda (info)
                                 (length (plist-get info :prompt-text))) model-info-list)))
               (max-output-width
                (apply #'max
                       (mapcar (lambda (info)
                                 (length (plist-get info :output-text))) model-info-list)))
               (format-str (format "[%%-%ds] %%-%ds%%-%ds (Tokens in/out: %%%ds/%%%ds)"
                                   max-vendor-width
                                   max-name-width
                                   max-preview-width
                                   max-prompt-width
                                   max-output-width)))
          ;; Return list of (name . id) pairs from fetched models, sorted by ID
          (sort
           (mapcar (lambda (info)
                     (cons
                      (format format-str
                              (plist-get info :vendor)
                              (plist-get info :clean-name)
                              (plist-get info :preview-text)
                              (plist-get info :prompt-text)
                              (plist-get info :output-text))
                      (plist-get info :id)))
                   model-info-list)
           (lambda (a b) (string< (cdr a) (cdr b)))))
      ;; No models available - fetch and wait
      (progn
        ;; Try loading from cache first
        (let ((cached-models (copilot-chat--load-models-from-cache)))
          (if cached-models
              (progn
                (setf (copilot-chat-connection-models copilot-chat--connection)
                      cached-models)
                (copilot-chat--get-model-choices-with-wait))
            ;; No cache - need to fetch
            (message "No models available. Fetching from API...")
            (copilot-chat--auth)
            (let ((inhibit-quit t))  ; Prevent C-g during fetch
              (copilot-chat--request-models t)
              ;; Wait for models to be fetched (with timeout)
              (with-timeout (10 (error "Timeout waiting for models to be fetched"))
                (while (not (copilot-chat-connection-models copilot-chat--connection))
                  (sit-for 0.1)))
              (copilot-chat--get-model-choices-with-wait))))))))

;;;###autoload (autoload 'copilot-chat-set-model "copilot-chat" nil t)
(defun copilot-chat-set-model (model)
  "Set the Copilot Chat model to MODEL for the current instance.
Fetches available models from the API if not already fetched."
  (interactive
   (let* ((choices (copilot-chat--get-model-choices-with-wait))
          (max-id-width
           (apply #'max
                  (mapcar (lambda (choics)
                            (length (cdr choics))) choices)))
          ;; Create completion list with ID as prefix for unique identification
          (completion-choices
           (mapcar (lambda (choice)
                     (let ((name (car choice))
                           (id (cdr choice)))
                       (cons (format (format "[%%-%ds] %%s" max-id-width)
                                     id name)
                             id)))
                   choices))
          (choice
           (completing-read "Select Copilot Chat model: "
                            (mapcar 'car completion-choices)
                            nil t)))
     ;; Extract model ID from the selected choice
     (let ((model-value (cdr (assoc choice completion-choices))))
       (when copilot-chat-debug
         (message "Setting model to: %s" model-value))
       (list model-value))))

  ;; Set the model value only for current instance
  (let ((instance (copilot-chat--current-instance)))
    (setf (copilot-chat-model instance) model)
    (message "Copilot Chat model set to %s for current instance" model)))

(defun copilot-chat-yank()
  "Insert last code block given by `copilot-chat'."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (setf
     (copilot-chat-yank-index instance) 1
     (copilot-chat-last-yank-start instance) nil
     (copilot-chat-last-yank-end instance) nil)
    (copilot-chat--yank instance)))

(defun copilot-chat-yank-pop(&optional inc)
  "Replace just-yanked code block with a different block.
INC is the number to use as increment for index in block ring."
  (interactive "*p")
  (let ((instance (copilot-chat--current-instance)))
    (if (not (eq last-command 'copilot-chat-yank-pop))
        (unless (eq last-command 'copilot-chat-yank)
          (error "Previous command was not a yank")))
    (if inc
        (setf (copilot-chat-yank-index instance)
              (+ (copilot-chat-yank-index instance) inc))
      (setf (copilot-chat-yank-index instance)
            (1+ (copilot-chat-yank-index instance))))
    (copilot-chat--yank instance)
    (setq this-command 'copilot-chat-yank-pop)))

(defun copilot-chat--yank(instance)
  "Insert at point the code block at the current index in the block ring.
Argument INSTANCE is the copilot chat instance to use."
  (when-let* ((yank-fn (copilot-chat-frontend-yank-fn (copilot-chat--get-frontend))))
    (funcall yank-fn instance)))

;;;###autoload (autoload 'copilot-chat-clear-auth-cache "copilot-chat" nil t)
(defun copilot-chat-clear-auth-cache ()
  "Clear the auth cache for Copilot Chat."
  (interactive)
  (copilot-chat-reset)
  ;; remove copilot-chat-token-cache file
  (let ((token-cache-file (expand-file-name copilot-chat-token-cache))
        (github-token-file (expand-file-name copilot-chat-github-token-file)))
    (when (file-exists-p token-cache-file)
      (delete-file token-cache-file))
    (when (file-exists-p github-token-file)
      (delete-file github-token-file)))
  (message "Auth cache cleared.")
  (setq copilot-chat--connection (copilot-chat-connection--make)))

;;;###autoload (autoload 'copilot-chat-reset-models "copilot-chat" nil t)
(defun copilot-chat-reset-models ()
  "Reset model cache and fetch models again.
This is useful when GitHub adds new models or updates model capabilities.
Clears model cache from memory and disk, then triggers background fetch."
  (interactive)
  ;; Clear models
  (setf (copilot-chat-connection-models copilot-chat--connection) nil)
  (setf (copilot-chat-connection-last-models-fetch-time copilot-chat--connection) 0)

  ;; Remove cached models file if it exists
  (let ((models-cache-file (expand-file-name copilot-chat-models-cache-file)))
    (when (file-exists-p models-cache-file)
      (delete-file models-cache-file)
      (when copilot-chat-debug
        (message "Removed models cache file: %s" models-cache-file))))

  ;; Trigger a background fetch
  (message "Models cache cleared. Fetching updated models...")
  (copilot-chat--fetch-models-async)

  ;; Return nil for programmatic usage
  nil)

(aio-defun copilot-chat--add-workspace (instance only-open)
  "Add all files matching an instance to buffer list.
INSTANCE is the copilot chat instance to use.  If ONLY-OPEN is non-nil,
add only files already visited by a buffer."
  (copilot-chat--clear-buffers instance)
  (let* ((default-directory (copilot-chat-directory instance))
         (repo-root (aio-await (copilot-chat--git-top-level)))
         (files (if (null repo-root)
                    (directory-files-recursively default-directory ".*" nil)
                  (aio-await (copilot-chat--git-ls-files repo-root))))
         (file-count (length files)))
    (if (and (> file-count 50)
             (not only-open)
             (not (yes-or-no-p (format "Found %d files, add them all? "
                                       file-count))))
        (message "Workspace file addition cancelled.")
      (if only-open
          (mapcar
           (lambda (buffer)
             (let ((file (buffer-file-name buffer)))
               (when (and file
                          (member (expand-file-name file) files))
                 (copilot-chat--add-buffer instance buffer))))
           (buffer-list))
        (mapcar
         (lambda (file)
           (copilot-chat-add-file file))
         files)))))

(defun copilot-chat-add-workspace-buffers ()
  "Add all open files matching an instance.
If a gitignore file is present in the instance directory, it will be used to
filter files.  Buffer list is cleared and all buffer displaying a file in the
instance directory will be added."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (aio-with-async
      (aio-await (copilot-chat--add-workspace instance t))
      (copilot-chat-list-refresh instance))))

(defun copilot-chat-add-workspace ()
  "Add all files in instance's directory.
All files in instance's directory and its subdirectories are added to
context.  If a gitignore file is present in the instance directory, it
will be used to filter files.  Buffer list is cleared and all buffer
displaying a file in the instance directory will be added."
  (interactive)
  (let ((instance (copilot-chat--current-instance)))
    (aio-with-async
      (aio-await (copilot-chat--add-workspace instance nil))
      (copilot-chat-list-refresh instance))))

(provide 'copilot-chat-command)
;;; copilot-chat-command.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
