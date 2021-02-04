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
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(provide 'extras)
