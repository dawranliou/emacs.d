;;; -*- lexical-binding: t; -*-

(defun yank-pop+ ()
  "If there is a recent yank act like `yank-pop'.
Otherwise choose text from the kill ring and insert it."
  (interactive)
  (if (eq last-command 'yank)
      (yank-pop)
    (let ((text (completing-read "Yank Ring: "
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
