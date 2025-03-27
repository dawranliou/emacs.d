;;; jira-detail.el --- Jira Detail  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Show information about a specific Jira elements.

;;; Code:

(require 'magit-section)

(require 'jira-fmt)
(require 'jira-api)
(require 'jira-table)
(require 'jira-doc)
(require 'jira-utils)

;; they override the default formatters specified
;; in jira-issues-fields
(defvar jira-detail-formatters
  '((:key . jira-fmt-issue-key-not-tabulated)
    (:parent-key . jira-fmt-issue-key-not-tabulated)))

(defun jira-detail--header (header)
  "Format HEADER to be used as a header in the detail view."
  (concat
   (propertize header 'face 'italic)
   (when (> (- 16 (string-width header)) 0)
     (make-string (- 16 (string-width header)) ?\s))))

(defun jira-detail--issue-fmt (issue field)
  "Extract FIELD from ISSUE and format it."
  (let* ((extracted (jira-table-extract-field jira-issues-fields field issue))
         (value (format "%s" (or extracted "")))
         (formatter
          (or (cdr (assoc field jira-detail-formatters))
              (jira-table-field-formatter jira-issues-fields field))))
    (if formatter (funcall formatter value) value)))

(defun jira-detail--issue-summary (issue)
  "Show the summary of the ISSUE."
  (let ((key (jira-detail--issue-fmt issue :key))
        (type (jira-detail--issue-fmt issue :issue-type-name))
        (status (jira-detail--issue-fmt issue :status-name))
        (resolution (jira-detail--issue-fmt issue :resolution))
        (project (jira-detail--issue-fmt issue :project-name))
        (estimate (jira-detail--issue-fmt issue :original-estimate))
        (remaining (jira-detail--issue-fmt issue :remaining-time))
        (ratio (jira-detail--issue-fmt issue :work-ratio))
        (fix-versions (jira-detail--issue-fmt issue :fix-versions))
        (summary (jira-detail--issue-fmt issue :summary)))
    (insert (jira-detail--header "Issue Key") key " (" type")\n")
    (insert (jira-detail--header "Status") status)
    (if (and resolution (not (string= resolution "")))
        (insert " (" resolution ")\n")
      (insert "\n"))
    (insert (jira-detail--header "Project") project "\n")
    (insert (jira-detail--header "Time")
            estimate
            " (remaining: " remaining ")\n")
    (insert (jira-detail--header "Work Ratio") ratio "\n")
    (insert (jira-detail--header "Fix Versions") fix-versions "\n")
    (insert (jira-detail--header "Summary") summary "\n")))


(defun jira-detail--issue-team (issue)
  "Show the team information of the ISSUE."
  (let ((line (jira-detail--issue-fmt issue :line))
        (parent-key (jira-detail--issue-fmt issue :parent-key))
        (parent-type (jira-detail--issue-fmt issue :parent-type-name))
        (parent-status (jira-detail--issue-fmt issue :parent-status))
        (sprint (jira-detail--issue-fmt issue :sprints))
        (components (jira-detail--issue-fmt issue :components)))
    (insert (jira-detail--header "Business Line") line "\n")
    (insert (jira-detail--header "Parent")
            parent-key " (" parent-type ") (" parent-status ")\n")
    (insert (jira-detail--header "Sprint") sprint "\n")
    (insert (jira-detail--header "Components") components "\n")))

(defun jira-detail--issue-manager-data (issue)
  "Show the manager data of the ISSUE."
  (let ((cost-center (jira-detail--issue-fmt issue :cost-center))
        (priority (jira-detail--issue-fmt issue :priority-name))
        (labels (jira-detail--issue-fmt issue :labels))
        (assignee (jira-detail--issue-fmt issue :assignee-name))
        (reporter (jira-detail--issue-fmt issue :reporter-name))
        (due-date (jira-detail--issue-fmt issue :due-date)))
    (insert (jira-detail--header "Cost Center") cost-center "\n")
    (insert (jira-detail--header "Priority") priority "\n")
    (insert (jira-detail--header "Labels") labels "\n")
    (insert (jira-detail--header "Assignee")  assignee "\n")
    (insert (jira-detail--header "Reporter") reporter "\n")
    (insert (jira-detail--header "Due Date") due-date "\n")))

(defun jira-detail--description (issue)
  "Show the description of the ISSUE."
  (let* ((doc (alist-get 'description (alist-get 'fields issue))))
    (insert  (jira-doc-format doc))))

(cl-defun jira-detail--issue (key issue)
  "Show the detail information of the ISSUE with KEY."
  (with-current-buffer (get-buffer-create (concat "*Jira Issue Detail: [" key "]*"))
    (magit-section-mode)
    ;; avoid horizontal scroll
    (setq truncate-lines nil)
    (visual-line-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; section: Jira Issue Summary
      (magit-insert-section (jira-issue-summary nil nil)
        (magit-insert-heading "Jira Issue Summary")
        (magit-insert-section-body (jira-detail--issue-summary issue)))
      (insert "\n")
      ;; section: Information
      (magit-insert-section (jira-issue-information nil nil)
        ;; section: Team Data
        (magit-insert-section (team-data nil nil)
          (magit-insert-heading "Team Data")
          (magit-insert-section-body (jira-detail--issue-team issue))
          (insert "\n"))
        ;; section: Manager Data
        (magit-insert-section (manager-data nil nil)
          (magit-insert-heading "Manager Data")
          (magit-insert-section-body
            (jira-detail--issue-manager-data issue)
            (insert "\n")))
        (magit-insert-section (description nil nil)
          (magit-insert-heading "Description")
          (magit-insert-section-body
            (jira-detail--description issue)))))
    (pop-to-buffer (current-buffer))))

(defun jira-detail-show-issue (key)
  "Retrieve and show the detail information of the issue with KEY."
  (jira-api-call
   "GET" (concat "issue/" key)
   :callback
   (lambda (data _response)
     (let* ((issue (json-read-from-string (json-encode data))))
       (jira-detail--issue key issue)))))


(provide 'jira-detail)

;;; jira-detail.el ends here
