;;; jira-issues.el ---  Actions on items  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo
;; Created: 2025-02-16

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>

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

;; Manage actions on Jira objects

;;; Code:

(require 'transient)
(require 'tablist)

(require 'jira-api)
(require 'jira-utils)
(require 'jira-tempo)

(defun jira-actions--marked-issue-description ()
  "TODO: This will change if the list of issues fields is changed."
  (let ((vector (cdr (car (tablist-get-marked-items)))))
    (aref vector (1- (length vector)))))

(defun jira-actions--add-worklog ()
  "Add a worklog to marked issue."
  (let* ((args (transient-args 'jira-actions-add-worklog-menu))
         (notify (transient-arg-value "--notify-users" args))
         (comment (transient-arg-value "--comment=" args))
         (estimate (transient-arg-value "--new-estimate=" args))
         (date (transient-arg-value "--date=" args))
         (time (transient-arg-value "--time=" args))
         (adjust-estimate
          (if (and estimate (not (string-empty-p estimate))) "new" "auto")))
    (jira-api-call
     "POST" (concat "issue/"(jira-utils-marked-item) "/worklog")
     :params `(("notifyUsers" . ,(if notify "true" "false"))
               ("adjustEstimate" . ,adjust-estimate)
               ,@(when (string= adjust-estimate "new")
                   `(("newEstimate" . ,estimate))))
     :data `(("started" . ,date)
             ("comment" . (("type" . "doc")
                           ("version" . 1)
                           ("content" . ((("type" . "paragraph")
                                          ("content" . ((("type" . "text")
                                                         ("text" . ,comment)))))))))
             ("timeSpent" . ,time))
     :callback (lambda (_data _response) (jira-tempo)))))

(transient-define-prefix jira-actions-add-worklog-menu ()
  "Show menu for adding a Worklog to a Jira Issue."
  :value (lambda () `
           (,(concat "--comment=" (jira-utils-marked-item)
                     ": " (jira-actions--marked-issue-description))
            ,(concat "--date="
                     (format-time-string "%FT%T.%3N%z" (current-time)))
            "--time=1h"))
  ["Arguments"
   ("n" "Notify users" "--notify-users")
   ("e" "New estimate" "--new-estimate=")
   ("c" "Comment" "--comment=")
   ("d" "Date" "--date="
    :reader (lambda (&rest _)
	      (completing-read
	       "Choose an option: "
	       (jira-utils-last-n-dates (current-time) 7)
	       nil nil nil 'transient-history)))
  ("t" "Time Spent" "--time=")]
  ["Actions"
   ("a" "Add worklog"
    (lambda () (interactive) (jira-actions--add-worklog)))]

  (interactive)
  (if (jira-utils-marked-item)
      (transient-setup 'jira-actions-add-worklog-menu)
    (message "Run jira-issues first")))

(defun jira-actions--change-issue ()
  "Apply user-selected options to marked issue."
  (let* ((args (transient-args 'jira-actions-change-issue-menu))
         (status (transient-arg-value "--status=" args))
         (resolution (transient-arg-value "--resolution=" args))
         (status-id (cdr (assoc status jira-active-issue-transitions)))
         (time-estimate (transient-arg-value "--time-estimate=" args))
         (hook (lambda (_data _response) (run-hooks 'jira-issues-changed-hook))))
    (when status-id
      (jira-api-call
       "POST" (concat "issue/" (jira-utils-marked-item) "/transitions")
       :data `(("transition" . (("id" . ,status-id))))
       :callback hook))
    (when (or resolution time-estimate)
      (jira-api-call
       "PUT" (concat "issue/" (jira-utils-marked-item))
       :data `(("fields" .
                ,(let (fields)
                   (when resolution
                     (push `( "resolution" . (("name" . ,resolution))) fields))
                   (when time-estimate
                     (push `( "timetracking" . (("originalEstimate" . ,time-estimate))) fields))
                   fields)))
       :callback hook))))

(transient-define-prefix jira-actions-change-issue-menu ()
  "Show menu for updating a Jira Issue."
  ["Arguments"
   ("s" "Status" "--status="
    :choices
    (lambda () (mapcar (lambda (tr) (car tr)) jira-active-issue-transitions)))
   ("r" "Resolution" "--resolution="
    :choices
    (lambda () (mapcar (lambda (res) (car res)) jira-resolutions)))
   ("e" "Time Estimate" "--time-estimate=")]
  ["Actions"
   ("c" "Change" (lambda () (interactive) (jira-actions--change-issue)))]

  (interactive)
  (if (jira-utils-marked-item)
      (progn  ;; retrieve possible statuses transtions
        (jira-api-get-transitions (jira-utils-marked-item))
        (transient-setup 'jira-actions-change-issue-menu))
    (message "Run jira-issues first")))

(defun jira-actions-open-issue (issue-key)
  "Open ISSUE-KEY in browser."
  (browse-url (format "%s/browse/%s" jira-base-url issue-key)))

(provide 'jira-actions)

;;; jira-actions.el ends here
