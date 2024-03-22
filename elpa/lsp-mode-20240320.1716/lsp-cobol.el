;;; lsp-cobol.el --- COBOL support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; COBOL support.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-cobol nil
  "LSP support for COBOL."
  :group 'lsp-mode
  :link '(url-link "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol")
  :package-version `(lsp-mode . "8.0.1"))

(defcustom lsp-cobol-server-path nil
  "Path points for COBOL language service.

This is only for development use."
  :type 'string
  :group 'lsp-cobol)

(defcustom lsp-cobol-port 1044
  "Port to connect server to."
  :type 'integer
  :group 'lsp-cobol)

;;
;;; Util

(defmacro lsp-cobol--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun lsp-cobol--execute (cmd &rest args)
  "Return non-nil if CMD executed succesfully with ARGS."
  (save-window-excursion
    (lsp-cobol--mute-apply
      (= 0 (shell-command (concat cmd " "
                                  (mapconcat #'shell-quote-argument
                                             (cl-remove-if #'null args)
                                             " ")))))))

;;
;;; Installation

(defcustom lsp-cobol-server-store-path
  (expand-file-name "cobol/" lsp-server-install-dir)
  "The path to the file in which COBOL language service will be stored."
  :type 'file
  :group 'lsp-cobol)

(defcustom lsp-cobol-server-version "2.1.1"
  "The COBOL language service version to install."
  :type 'file
  :group 'lsp-cobol)

(defconst lsp-cobol-download-url-format
  "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol/releases/download/%s/cobol-language-support-%s-%s-%s%s.vsix"
  "Format to the download url link.")

(defun lsp-cobol--server-url ()
  "Return Url points to the cobol language service's zip/tar file."
  (let* ((x86 (string-prefix-p "x86_64" system-configuration))
         (arch (if x86 "x64" "arm64"))
         (version lsp-cobol-server-version))
    (cl-case system-type
      ((cygwin windows-nt ms-dos)
       (format lsp-cobol-download-url-format
               version "win32" arch version "-signed"))
      (darwin
       (format lsp-cobol-download-url-format
               version "darwin" arch version ""))
      (gnu/linux
       (format lsp-cobol-download-url-format
               version "linux" arch version "")))))

(defvar lsp-cobol--server-download-url (lsp-cobol--server-url)
  "The actual url used to download language server.")

(defvar lsp-cobol--downloaded-file (f-join lsp-cobol-server-store-path "temp.tar")
  "The full file path after downloading the server zipped file.")

(defun lsp-cobol--extract-compressed-file (callback)
  "Install COBOL language service."
  (cond ((file-exists-p lsp-cobol--downloaded-file)
         ;; Suprisingly, you can just use `tar' to unzip a zip file on Windows.
         ;; Therefore, just use the same command.
         (lsp-cobol--execute "tar" "-xvzf" lsp-cobol--downloaded-file "-C" lsp-cobol-server-store-path)
         ;; Delete the zip file.
         (ignore-errors (delete-file lsp-cobol--downloaded-file)))
        (t
         (error "Can't extract the downloaded file: %s" lsp-cobol--downloaded-file)))
  (funcall callback))

(defun lsp-cobol--stored-executable ()
  "Return the stored COBOL language service executable."
  (executable-find
   (f-join lsp-cobol-server-store-path
           (concat "extension/server/native/"
                   (cl-case system-type
                     ((cygwin windows-nt ms-dos) "engine.exe")
                     (darwin                     "server-mac")
                     (gnu/linux                  "server-linux"))))))

(lsp-dependency
 'cobol-ls
 '(:system "cobol-ls")
 `(:download :url ,lsp-cobol--server-download-url
             :store-path ,lsp-cobol--downloaded-file))

;;
;;; Server

;;;###autoload
(add-hook 'cobol-mode-hook #'lsp-cobol-start-ls)

;;;###autoload
(defun lsp-cobol-start-ls ()
  "Start the COBOL language service."
  (interactive)
  (when-let ((exe (lsp-cobol--executable))
             ((lsp--port-available "localhost" lsp-cobol-port)))
    (lsp-async-start-process #'ignore #'ignore exe)))

;;
;;; Core

(defun lsp-cobol--executable ()
  "Return the COBOL language service executable."
  (or lsp-cobol-server-path
      (lsp-cobol--stored-executable)))

(defun lsp-cobol-server-start-fn (&rest _)
  "Define COOBL language service start function."
  `(,(lsp-cobol--executable)))

(defun lsp-cobol--tcp-connect-to-port ()
  "Define a TCP connection to language server."
  (list
   :connect
   (lambda (filter sentinel name _environment-fn _workspace)
     (let* ((host "localhost")
            (port lsp-cobol-port)
            (tcp-proc (lsp--open-network-stream host port (concat name "::tcp"))))

       ;; TODO: Same :noquery issue (see above)
       (set-process-query-on-exit-flag tcp-proc nil)
       (set-process-filter tcp-proc filter)
       (set-process-sentinel tcp-proc sentinel)
       (cons tcp-proc tcp-proc)))
   :test? (lambda () (file-executable-p (lsp-cobol--executable)))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-cobol--tcp-connect-to-port)
  :activation-fn (lsp-activate-on "cobol")
  :priority -1
  :server-id 'cobol-ls
  :download-server-fn
  (lambda (_client callback error-callback _update?)
    (lsp-package-ensure 'cobol-ls
                        (lambda () (lsp-cobol--extract-compressed-file callback))
                        error-callback))))

(lsp-consistency-check lsp-cobol)

(provide 'lsp-cobol)
;;; lsp-cobol.el ends here
