;;; -*- lexical-binding: t; -*-

(defun my/set-font ()
  "Select xfont."
  (interactive)
  (set-frame-font (completing-read
                   "Choose font:"
                   (cl-remove-duplicates (x-list-fonts "*") :test #'equal))))


(defun yank-pop+ ()
  "If there is a recent yank act like `yank-pop'.
Otherwise choose text from the kill ring and insert it."
  (interactive)
  (if (eq last-command 'yank)
      (yank-pop)
    (let* ((selectrum-should-sort-p nil)
           (text (completing-read "Yank Ring: "
                                  (cl-remove-duplicates kill-ring :test #'equal :from-end t)
                                  nil ':require-match)))
      (setq yank-window-start (window-start))
      (push-mark)
      (insert-for-yank text)
      (setq this-command 'yank)
      nil)))


(defun recentf-open-files+ ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (unless recentf-mode
    (recentf-mode +1))
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))


(defun +move-beginning-of-line (arg)
  "Move point to beginning of current line or the first non whitespace char."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Move by lines, if ARG is not 1 (the default).
  (if (/= arg 1)
      (let ((line-move-visual nil))
        (line-move (1- arg) t)))

  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))


(defun +newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))


(defun +uuid ()
  "Generate a new UUID and insert."
  (interactive)
  (insert (downcase (string-trim (shell-command-to-string "uuidgen")))))


(defun +kill-line-backwards ()
  "Kill line backwards."
  (interactive)
  (kill-line 0))


(defun +eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))


(defun +backward-kill-word-or-region (&optional arg)
  "Kill word backwards unless region is active,
kill region instead"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word (or arg 1))))


(defun +toggle-window-split ()
  "Toggle window split from vertical to horizontal."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows.")
    (let ((was-full-height (window-full-height-p)))
      (delete-other-windows)
      (if was-full-height
          (split-window-vertically)
        (split-window-horizontally))
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))


(defun +transpose-windows ()
  "Swap the buffers shown in current and next window."
  (interactive)
  (let ((this-buffer (window-buffer))
        (next-window (next-window nil :no-minibuf nil)))
    (set-window-buffer nil (window-buffer next-window))
    (set-window-buffer next-window this-buffer)
    (select-window next-window)))


(defun +unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))


(defun +fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
        With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))


(defun +-kill-and-echo (X)
  "Copy `X' into the `kill-ring' and echo to the minibuffer."
  (kill-new X)
  (message "[COPIED] %s" X))


(defun +copy-path ()
  "Echo file name to minibuffer and copy to kill ring."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
        (proj (project-current nil)))
    (if proj
        (let* ((proj-path (expand-file-name (cdr proj)))
               (filename* (nth 1 (split-string filename proj-path))))
          (+-kill-and-echo filename*))
      (+-kill-and-echo filename))))


(provide 'extras)
