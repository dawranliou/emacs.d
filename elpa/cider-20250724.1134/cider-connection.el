;;; cider-connection.el --- Connection and session life-cycle management for CIDER -*- lexical-binding: t -*-
;;
;; Copyright © 2019-2025 Artur Malabarba, Bozhidar Batsov, Vitalie Spinu and CIDER contributors
;;
;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;;         Bozhidar Batsov <bozhidar@batsov.dev>
;;         Vitalie Spinu <spinuvit@gmail.com>
;;
;; Keywords: languages, clojure, cider
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'nrepl-client)
(require 'cl-lib)
(require 'format-spec)
(require 'sesman)
(require 'sesman-browser)
(require 'spinner)
(require 'cider-popup)
(require 'cider-util)

(defcustom cider-session-name-template "%J:%h:%p"
  "Format string to use for session names.
See `cider-format-connection-params' for available format characters."
  :type 'string
  :group 'cider
  :package-version '(cider . "0.18.0"))

(defcustom cider-redirect-server-output-to-repl  t
  "Controls whether nREPL server output would be redirected to the REPL.
When non-nil the output would end up in both the nrepl-server buffer (when
available) and the matching REPL buffer."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.17.0"))

(defcustom cider-auto-mode t
  "When non-nil, automatically enable and disable CIDER in all Clojure buffers.

After an initial connection, `cider-mode' is added to `clojure-mode-hook' and
automatically enabled on all existing Clojure buffers.  After the last
connection has been closed, `cider-mode' is disabled in all Clojure buffers, and
has to be manually re-enabled via \\[cider-mode].

Useful for switching between alternative minor modes like `inf-clojure-mode'."
  :type 'boolean
  :group 'cider
  :safe #'booleanp
  :package-version '(cider . "0.9.0"))

(defcustom cider-merge-sessions nil
  "Controls session combination behavior.

Symbol `host' combines all sessions of a project associated with the same host.
Symbol `project' combines all sessions of a project.

All other values do not combine any sessions."
  :type '(choice (const :tag "Combine all sessions with the same host" host)
                 (const :tag "Combine all sessions from the same project" project)
                 (other :tag "Do not combine any sessions"))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "1.5"))

(defcustom cider-reuse-dead-repls 'prompt
  "How to deal with existing dead REPL buffers when initializing a connection.

Possible choices are `prompt', `auto', `any', and nil.
- `prompt' means to always ask the user for a decision.
- `auto' means to automatically reuse a dead REPL without prompting the user
  if it is the only available option.  When there are multiple buffers to
  choose from, the user is is prompted for a choice.
- `any' (or any other non-nil value) means to reuse any dead REPL buffer
  available, by default the most relevant according to various heuristics,
  and never prompt the user.
- nil means to start a new REPL each time, ignoring existing buffers."
  :type '(choice (const :tag "Always prompt for what to do with dead REPLs" prompt)
                 (const :tag "Reuse dead REPL, prompting only for multiple choice" auto)
                 (const :tag "Reuse any available dead REPL and never prompt" any)
                 (const :tag "Never reuse dead REPLs" nil))
  :group 'cider
  :safe #'symbolp
  :package-version '(cider . "1.8"))

(defconst cider-required-nrepl-version "0.6.0"
  "The minimum nREPL version that's known to work properly with CIDER.")

(defcustom cider-clojurec-eval-destination 'multi
  "The REPL type to be chosen in .cljc buffers."
  :type '(choice (const :tag "Clojure" clj)
                 (const :tag "ClojureScript" cljs)
                 (const :tag "Multi (evaluate in Clojure and ClojureScript simultaneously)" multi))
  :group 'cider
  :package-version '(cider . "1.8"))

;;; Connect

(defun cider-nrepl-connect (params)
  "Start nrepl client and create the REPL.
PARAMS is a plist containing :host, :port, :server and other parameters for
`cider-repl-create'."
  (process-buffer
   (nrepl-start-client-process
    (plist-get params :host)
    (plist-get params :port)
    (plist-get params :server)
    (lambda (_)
      (cider-repl-create params))
    (plist-get params :socket-file))))

(defun cider-sessions ()
  "Return a list of all active CIDER sessions."
  (sesman-sessions 'CIDER))

(defun cider-connected-p ()
  "Return t if CIDER is currently connected, nil otherwise."
  (process-live-p (get-buffer-process (cider-current-repl))))

(defun cider-ensure-connected ()
  "Ensure there is a linked CIDER session."
  (sesman-ensure-session 'CIDER))

(defun cider--session-server (session)
  "Return server buffer for SESSION or nil if there is no server."
  (seq-some (lambda (r)
              (buffer-local-value 'nrepl-server-buffer r))
            (cdr session)))

(defun cider--gather-session-params (session)
  "Gather all params for a SESSION."
  (let (params)
    (dolist (repl (cdr session))
      (setq params (cider--gather-connect-params params repl)))
    (when-let* ((server (cider--session-server session)))
      (setq params (cider--gather-connect-params params server)))
    params))

(defun cider--gather-connect-params (&optional params proc-buffer)
  "Gather all relevant connection parameters into PARAMS plist.
PROC-BUFFER is either server or client buffer, defaults to current buffer."
  (let ((proc-buffer (or proc-buffer (current-buffer))))
    (with-current-buffer proc-buffer
      (unless nrepl-endpoint
        (error "This is not a REPL or SERVER buffer; is there an active REPL?"))
      (let ((server-buf (if (nrepl-server-p proc-buffer)
                            proc-buffer
                          nrepl-server-buffer)))
        (cl-loop for l on nrepl-endpoint by #'cddr
                 do (setq params (plist-put params (car l) (cadr l))))
        (setq params (thread-first params
                                   (plist-put :project-dir nrepl-project-dir)))
        (when (buffer-live-p server-buf)
          (setq params (thread-first params
                                     (plist-put :server (get-buffer-process server-buf))
                                     (plist-put :server-command nrepl-server-command))))
        ;; repl-specific parameters (do not pollute server params!)
        (unless (nrepl-server-p proc-buffer)
          (setq params (thread-first params
                                     (plist-put :session-name cider-session-name)
                                     (plist-put :repl-type cider-repl-type)
                                     (plist-put :cljs-repl-type cider-cljs-repl-type)
                                     (plist-put :repl-init-function cider-repl-init-function))))
        params))))

(defun cider--close-buffer (buffer)
  "Close the BUFFER and kill its associated process (if any)."
  (when (buffer-live-p buffer)
    (when-let* ((proc (get-buffer-process buffer)))
      (when (process-live-p proc)
        (delete-process proc)))
    (kill-buffer buffer)))

(declare-function cider-repl-emit-interactive-stderr "cider-repl")
(defun cider--close-connection (repl &optional no-kill)
  "Close connection associated with REPL.
When NO-KILL is non-nil stop the connection but don't kill the REPL
buffer."
  (when (buffer-live-p repl)
    (with-current-buffer repl
      (when spinner-current (spinner-stop))
      (when nrepl-tunnel-buffer
        (cider--close-buffer nrepl-tunnel-buffer))
      (when no-kill
        ;; inform sentinel not to kill the server, if any
        (thread-first (get-buffer-process repl)
                      (process-plist)
                      (plist-put :keep-server t))))
    (let ((proc (get-buffer-process repl)))
      (when (and (process-live-p proc)
                 (or (not nrepl-server-buffer)
                     ;; Sync request will hang if the server is dead.
                     (process-live-p (get-buffer-process nrepl-server-buffer))))
        (nrepl-sync-request:close repl)
        ;; give a chance to the REPL to respond to the closing of the connection
        (sleep-for 0.5)
        (delete-process proc)))
    (when-let* ((messages-buffer (and nrepl-log-messages
                                      (nrepl-messages-buffer repl))))
      (kill-buffer messages-buffer))
    (unless no-kill
      (kill-buffer repl)))
  (when repl
    (sesman-remove-object 'CIDER nil repl (not no-kill) t)))

(defun cider-emit-manual-warning (section-id format &rest args)
  "Emit a warning to the REPL and link to the online manual.
SECTION-ID is the section to link to.  The link is added on the last line.
FORMAT is a format string to compile with ARGS and display on the REPL."
  (let ((message (apply #'format format args)))
    (cider-repl-emit-interactive-stderr
     (concat "WARNING: " message " ("
             (cider--manual-button "More information" section-id)
             ")\n"))))

(defvar cider-version)
(defun cider--check-required-nrepl-version ()
  "Check whether we're using a compatible nREPL version."
  (if-let* ((nrepl-version (cider--nrepl-version)))
      (when (version< nrepl-version cider-required-nrepl-version)
        (cider-emit-manual-warning "troubleshooting.html#warning-saying-you-have-to-use-newer-nrepl"
                                   "CIDER requires nREPL %s (or newer) to work properly"
                                   cider-required-nrepl-version))))

(defvar cider-minimum-clojure-version)
(defun cider--check-clojure-version-supported ()
  "Ensure that we are meeting the minimum supported version of Clojure."
  (if-let* ((clojure-version (cider--clojure-version))
            ;; drop all qualifiers from the version string
            ;; e.g. 1.10.0-master-SNAPSHOT becomes simply 1.10.0
            (clojure-version (car (split-string clojure-version "-"))))
      (when (version< clojure-version cider-minimum-clojure-version)
        (cider-emit-manual-warning "basics/installation.html#prerequisites"
                                   "Clojure version (%s) is not supported (minimum %s). CIDER will not work."
                                   clojure-version cider-minimum-clojure-version))))

(defun cider--strip-version-patch (v)
  "Strips everything but major.minor from the version, returning a version list.
V is the version string to strip the patch from."
  (seq-take (version-to-list v) 2))

(defun cider--compatible-middleware-version-p (required-ver ver)
  "Checks that the available middleware version is compatible with the required.
We look only at the major and minor components.  When the major
version is 0, only check that the minor versions match.  When the major version
is > 0, first check that the major version matches, then that the minor
version is >= the required minor version.
VER the `installed' version,
REQUIRED-VER the version required by cider."
  (let ((ver* (cider--strip-version-patch ver))
        (required-ver* (cider--strip-version-patch required-ver)))
    (cond ((= 0 (car required-ver*))  (= (cadr required-ver*)
                                         (cadr ver*)))
          (t (and (= (car required-ver*)
                     (car ver*))
                  (version-list-<= required-ver* ver*))))))

(defvar cider-required-middleware-version)
(defun cider--check-middleware-compatibility ()
  "CIDER frontend/backend compatibility check.
Retrieve the underlying connection's CIDER-nREPL version and checks if the
middleware used is compatible with CIDER.  If not, will display a warning
message in the REPL area."
  (let* ((version-dict        (nrepl-aux-info "cider-version" (cider-current-repl)))
         (middleware-version  (nrepl-dict-get version-dict "version-string")))
    (cond
     ((null middleware-version)
      (cider-emit-manual-warning "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"
                                 "CIDER requires cider-nrepl to be fully functional. Some features will not be available without it!"))
     ((not (cider--compatible-middleware-version-p cider-required-middleware-version middleware-version))
      (cider-emit-manual-warning "troubleshooting.html#cider-complains-of-the-cider-nrepl-version"
                                 "CIDER %s requires cider-nrepl %s, but you're currently using cider-nrepl %s. The version mismatch might break some functionality!"
                                 cider-version cider-required-middleware-version middleware-version)))))

(declare-function cider-interactive-eval-handler "cider-eval")
(declare-function cider-nrepl-send-request "cider-client")
;; TODO: Use some null handler here
(defun cider--subscribe-repl-to-server-out ()
  "Subscribe to the nREPL server's *out*."
  (cider-nrepl-send-request '("op" "out-subscribe")
                            (cider-interactive-eval-handler (current-buffer))))

(defvar cider-mode)
(declare-function cider-mode "cider-mode")
(defun cider-enable-on-existing-clojure-buffers ()
  "Enable CIDER's minor mode on existing Clojure buffers.
See command `cider-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook #'cider-mode)
  (add-hook 'clojure-ts-mode-hook #'cider-mode)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (unless cider-mode
        (cider-mode +1)
        ;; In global-eldoc-mode, a new file-visiting buffer calls
        ;; `turn-on-eldoc-mode' which enables eldoc-mode if it's supported in that
        ;; buffer as determined by `eldoc--supported-p'.  Cider's eldoc support
        ;; allows new buffers in cider-mode to enable eldoc-mode.  As of 2021-04,
        ;; however, clojure-mode itself has no eldoc support, so old clojure
        ;; buffers opened before cider started aren't necessarily in eldoc-mode.
        ;; Here, we've enabled cider-mode for this old clojure buffer, and now, if
        ;; global-eldoc-mode is enabled, try to enable eldoc-mode as if the buffer
        ;; had just been created with cider-mode.
        (when global-eldoc-mode
          (turn-on-eldoc-mode))))))

(declare-function cider--debug-mode "cider-debug")
(defun cider-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' and related commands on existing Clojure buffers."
  (interactive)
  (dolist (buffer (cider-util--clojure-buffers))
    (with-current-buffer buffer
      (cider--debug-mode -1)
      (cider-mode -1))))

(defun cider-possibly-disable-on-existing-clojure-buffers ()
  "Disable `cider-mode' in all Clojure buffers if all CIDER sessions are closed."
  (unless (cider-sessions)
    (cider-disable-on-existing-clojure-buffers)))

(defun cider--set-connection-capabilities (&optional conn-buffer)
  "Set `cider-connection-capabilities' for CONN-BUFFER during repl init.
See `cider-connection-capabilities'."
  (with-current-buffer (or conn-buffer (current-buffer))
    (setf cider-connection-capabilities
          (append
           (pcase (cider-runtime)
             ('clojure '(clojure jvm-compilation-errors))
             ('babashka '(babashka jvm-compilation-errors))
             ('nbb '(cljs))
             ('scittle '(cljs))
             (_ '()))
           (when
               (eq cider-repl-type 'cljs)
             '(cljs))))))

(declare-function cider--debug-init-connection "cider-debug")
(declare-function cider-repl-init "cider-repl")
(declare-function cider-nrepl-op-supported-p "cider-client")
(declare-function cider-nrepl-request:eval "cider-client")

(defun cider--connected-handler ()
  "Handle CIDER initialization after nREPL connection has been established.
This function is appended to `nrepl-connected-hook' in the client process
buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer
  ;; `cider-enlighten-mode' changes eval to include the debugger, so we inhibit
  ;; it here as the debugger isn't necessarily initialized yet
  (let ((cider-enlighten-mode nil))
    ;; after initialization, set mode-line and buffer name.
    (cider-set-repl-type cider-repl-type)
    (cider-repl-init
     (current-buffer)
     (lambda ()
       ;; Init logic that's specific to Clojure's nREPL and cider-nrepl
       (when (cider-runtime-clojure-p)
         (cider--check-required-nrepl-version)
         (cider--check-clojure-version-supported)
         (cider--check-middleware-compatibility)

         ;; Redirect the nREPL's terminal output to a REPL buffer.
         ;; If we don't do this the server's output will end up
         ;; in the *nrepl-server* buffer.
         (when (and cider-redirect-server-output-to-repl
                    (cider-nrepl-op-supported-p "out-subscribe"))
           (cider--subscribe-repl-to-server-out))

         ;; Middleware on cider-nrepl's side is deferred until first usage, but
         ;; loading middleware concurrently can lead to occasional "require" issues
         ;; (likely a Clojure bug). Thus, we load the heavy debug middleware towards
         ;; the end, allowing for the faster "server-out" middleware to load
         ;; first.
         (cider--debug-init-connection))

       (cider--set-connection-capabilities)

       (when cider-repl-init-function
         (funcall cider-repl-init-function))

       (when cider-auto-mode
         (cider-enable-on-existing-clojure-buffers))

       (run-hooks 'cider-connected-hook)))))

(defun cider--disconnected-handler ()
  "Cleanup after nREPL connection has been lost or closed.
This function is appended to `nrepl-disconnected-hook' in the client
process buffer."
  ;; `nrepl-connected-hook' is run in the connection buffer
  (when cider-auto-mode
    (cider-possibly-disable-on-existing-clojure-buffers))
  (run-hooks 'cider-disconnected-hook))


;;; Connection Info

(defun cider--java-version ()
  "Retrieve the underlying connection's Java version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "java")
                    (nrepl-dict-get "version-string")))))

(defun cider--clojure-version ()
  "Retrieve the underlying connection's Clojure version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "clojure")
                    (nrepl-dict-get "version-string")))))

(defun cider--nrepl-version ()
  "Retrieve the underlying connection's nREPL version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (thread-first nrepl-versions
                    (nrepl-dict-get "nrepl")
                    (nrepl-dict-get "version-string")))))

(defun cider--babashka-version ()
  "Retrieve the underlying connection's Babashka version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "babashka"))))

(defun cider--babashka-nrepl-version ()
  "Retrieve the underlying connection's babashka.nrepl version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "babashka.nrepl"))))

(defun cider--nbb-nrepl-version ()
  "Retrieve the underlying connection's nbb version.

Note that this is currently not a real version number.
But helps us know if this is a nbb repl, or not."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "nbb-nrepl"))))

(defun cider--scittle-nrepl-version ()
  "Retrieve the underlying connection's scittle version."
  (with-current-buffer (cider-current-repl)
    (when nrepl-versions
      (nrepl-dict-get nrepl-versions "scittle-nrepl"))))

(defun cider-runtime ()
  "Return the runtime of the nREPl server."
  (cond
   ((cider--clojure-version) 'clojure)
   ((cider--babashka-version) 'babashka)
   ((cider--nbb-nrepl-version) 'nbb)
   ((cider--scittle-nrepl-version) 'scittle)
   (t 'generic)))

(defun cider-runtime-clojure-p ()
  "Check if the current runtime is Clojure."
  (eq (cider-runtime) 'clojure))

(defun cider--connection-info (connection-buffer &optional genericp)
  "Return info about CONNECTION-BUFFER.
Info contains project name, current REPL namespace, host:port endpoint and
runtime details.  When GENERICP is non-nil, don't provide specific info
about this buffer (like variable `cider-repl-type')."
  (with-current-buffer connection-buffer
    (cond
     ((cider--clojure-version)
      (format "%s%s@%s:%s (Java %s, Clojure %s, nREPL %s)"
              (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
              (or (cider--project-name nrepl-project-dir) "<no project>")
              (plist-get nrepl-endpoint :host)
              (plist-get nrepl-endpoint :port)
              (cider--java-version)
              (cider--clojure-version)
              (cider--nrepl-version)))
     ((cider--babashka-version)
      (format "%s%s@%s:%s (Babashka %s, babashka.nrepl %s)"
              (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
              (or (cider--project-name nrepl-project-dir) "<no project>")
              (plist-get nrepl-endpoint :host)
              (plist-get nrepl-endpoint :port)
              (cider--babashka-version)
              (cider--babashka-nrepl-version)))
     (t
      (format "%s%s@%s:%s"
              (if genericp "" (upcase (concat (symbol-name cider-repl-type) " ")))
              (or (cider--project-name nrepl-project-dir) "<no project>")
              (plist-get nrepl-endpoint :host)
              (plist-get nrepl-endpoint :port))))))

(defvar-local cider-connection-capabilities '()
  "A list of some of the capabilities of this connection buffer.
In other words - what assumptions we make about the runtime.
This is more general than
`cider-nrepl-op-supported-p' and `cider-library-present-p'.
But does not need to replace them.")

(defun cider-connection-has-capability-p (capability &optional conn-buf)
  "Return non nil when the cider connection has CAPABILITY for CONN-BUF.
By default it assumes the connection buffer is current."
  (with-current-buffer (or conn-buf (current-buffer))
    (member capability cider-connection-capabilities)))


;;; Connection Management Commands

(defun cider-quit (&optional repl)
  "Quit the CIDER connection associated with REPL.
REPL defaults to the current REPL."
  (interactive)
  (let ((repl (or repl
                  (sesman-browser-get 'object)
                  (cider-current-repl 'infer 'ensure))))
    (cider--close-connection repl))
  ;; if there are no more sessions we can kill all ancillary buffers
  (unless (cider-sessions)
    (cider-close-ancillary-buffers))
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))

(defun cider-restart (&optional repl)
  "Restart CIDER connection associated with REPL.
REPL defaults to the current REPL.  Don't restart the server or other
connections within the same session.  Use `sesman-restart' to restart the
entire session."
  (interactive)
  (let* ((repl (or repl
                   (sesman-browser-get 'object)
                   (cider-current-repl 'infer 'ensure)))
         (params (thread-first ()
                               (cider--gather-connect-params repl)
                               (plist-put :session-name (sesman-session-name-for-object 'CIDER repl))
                               (plist-put :repl-buffer repl))))
    (cider--close-connection repl 'no-kill)
    (cider-nrepl-connect params)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook)))

(defun cider-close-ancillary-buffers ()
  "Close buffers that are shared across connections."
  (interactive)
  (dolist (buf-name cider-ancillary-buffers)
    (when (get-buffer buf-name)
      (kill-buffer buf-name))))

(defun cider-describe-connection (&optional repl)
  "Display information about the connection associated with REPL.
REPL defaults to the current REPL."
  (interactive)
  (let ((repl (or repl
                  (sesman-browser-get 'object)
                  (cider-current-repl 'infer 'ensure))))
    (message "%s" (cider--connection-info repl))))

(defconst cider-nrepl-session-buffer "*cider-nrepl-session*")

(declare-function cider-nrepl-eval-session "cider-client")
(declare-function cider-nrepl-tooling-session "cider-client")
(defun cider-describe-nrepl-session ()
  "Describe an nREPL session."
  (interactive)
  (cider-ensure-connected)
  (let* ((repl (cider-current-repl 'infer 'ensure))
         (selected-session (completing-read "Describe nREPL session: " (nrepl-sessions repl))))
    (when (and selected-session (not (equal selected-session "")))
      (let* ((session-info (nrepl-sync-request:describe repl))
             (ops (nrepl-dict-keys (nrepl-dict-get session-info "ops")))
             (session-id (nrepl-dict-get session-info "session"))
             (session-type (cond
                            ((equal session-id (cider-nrepl-eval-session)) "Active eval")
                            ((equal session-id (cider-nrepl-tooling-session)) "Active tooling")
                            (t "Unknown"))))
        (with-current-buffer (cider-popup-buffer cider-nrepl-session-buffer 'select nil 'ancillary)
          (read-only-mode -1)
          (insert (format "Session: %s\n" session-id)
                  (format "Type: %s session\n" session-type)
                  (format "Supported ops:\n"))
          (mapc (lambda (op) (insert (format "  * %s\n" op))) ops)))
      (display-buffer cider-nrepl-session-buffer))))

(defun cider-list-nrepl-middleware ()
  "List the loaded nREPL middleware."
  (interactive)
  (cider-ensure-connected)
  (let* ((repl (cider-current-repl 'infer 'ensure))
         (middleware (nrepl-middleware repl)))
    (with-current-buffer (cider-popup-buffer "*cider-nrepl-middleware*" 'select nil 'ancillary)
      (read-only-mode -1)
      (insert (format "Currently loaded middleware:\n"))
      (mapc (lambda (mw) (insert (format "  * %s\n" mw))) middleware))
    (display-buffer "*cider-nrepl-middleware*")))


;;; Sesman's Session-Wise Management UI

(cl-defmethod sesman-project ((_system (eql CIDER)))
  "Find project directory."
  (clojure-project-dir (cider-current-dir)))

(cl-defmethod sesman-more-relevant-p ((_system (eql CIDER)) session1 session2)
  "Figure out if SESSION1 or SESSION2 is more relevant."
  (sesman-more-recent-p (cdr session1) (cdr session2)))

(declare-function cider-classpath-entries "cider-client")

(defvar cider-sesman-browser-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j q") #'cider-quit)
    (define-key map (kbd "j k") #'cider-quit)
    (define-key map (kbd "j r") #'cider-restart)
    (define-key map (kbd "j d") #'cider-describe-connection)
    (define-key map (kbd "j i") #'cider-describe-connection)
    (define-key map (kbd "C-c C-q") #'cider-quit)
    (define-key map (kbd "C-c C-r") #'cider-restart)
    (define-key map (kbd "C-c M-r") #'cider-restart)
    (define-key map (kbd "C-c C-d") #'cider-describe-connection)
    (define-key map (kbd "C-c M-d") #'cider-describe-connection)
    (define-key map (kbd "C-c C-i") #'cider-describe-connection)
    map)
  "Map active on REPL objects in sesman browser.")

(cl-defmethod sesman-session-info ((_system (eql CIDER)) session)
  "Obtain info for a CIDER SESSION."
  (list :objects (cdr session)
        :map cider-sesman-browser-map))

(declare-function cider "cider")
(cl-defmethod sesman-start-session ((_system (eql CIDER)))
  "Start a connection of any type interactively.
Fallback on `cider' command."
  (call-interactively #'cider))

(cl-defmethod sesman-quit-session ((_system (eql CIDER)) session)
  "Quit a CIDER SESSION."
  (mapc #'cider--close-connection (cdr session))
  ;; if there are no more session we can kill all ancillary buffers
  (unless (cider-sessions)
    (cider-close-ancillary-buffers)))

(cl-defmethod sesman-restart-session ((_system (eql CIDER)) session)
  "Restart a CIDER SESSION."
  (let* ((ses-name (car session))
         (repls (cdr session))
         (srv-buf (cider--session-server session)))
    (if srv-buf
        ;; session with a server
        (let ((s-params (cider--gather-connect-params nil srv-buf)))
          ;; 1) kill all connections, but keep the buffers
          (mapc (lambda (conn)
                  (cider--close-connection conn 'no-kill))
                repls)
          ;; 2) kill the server
          (nrepl-kill-server-buffer srv-buf)
          ;; 3) start server
          (nrepl-start-server-process
           (plist-get s-params :project-dir)
           (plist-get s-params :server-command)
           (lambda (server-buf)
             ;; 4) restart the repls reusing the buffer
             (dolist (r repls)
               (cider-nrepl-connect
                (thread-first ()
                              (cider--gather-connect-params r)
                              ;; server params (:port, :project-dir etc) have precedence
                              (cider--gather-connect-params server-buf)
                              (plist-put :session-name ses-name)
                              (plist-put :repl-buffer r))))
             (sesman-browser-revert-all 'CIDER)
             (message "Restarted CIDER %s session" ses-name))))
      ;; server-less session
      (dolist (r repls)
        (cider--close-connection r 'no-kill)
        (cider-nrepl-connect
         (thread-first ()
                       (cider--gather-connect-params r)
                       (plist-put :session-name ses-name)
                       (plist-put :repl-buffer r)))))))

(defun cider--ensure-spec-is-not-invokable (spec)
  "Ensures SPEC cannot be invoked as a function.

Invokeable specs are an Emacs 29 feature
that we don't intend to use in this context."
  (let ((spec-char (car spec))
        (spec-value (cdr spec)))
    `(,spec-char
      .
      ,(if (symbolp spec-value)
           (prin1-to-string spec-value)
         spec-value))))

(defun cider-format-connection-params (template params)
  "Format PARAMS with TEMPLATE string.
The following formats can be used in TEMPLATE string:

  %h - host
  %H - remote host, empty for local hosts
  %p - port
  %j - short project name, or directory name if no project
  %J - long project name including parent dir name
  %r - REPL type (clj or cljs)
  %S - type of the ClojureScript runtime (Browser, Node, Figwheel etc.)
  %s - session name as defined by `cider-session-name-template'.

In case some values are empty, extra separators (: and -) are automatically
removed."
  (let* ((dir (directory-file-name
               (abbreviate-file-name
                (or (plist-get params :project-dir)
                    (clojure-project-dir (cider-current-dir))
                    default-directory))))
         (short-proj (file-name-nondirectory (directory-file-name dir)))
         (parent-dir (ignore-errors
                       (thread-first dir
                                     file-name-directory
                                     directory-file-name file-name-nondirectory
                                     file-name-as-directory)))
         (long-proj (format "%s%s" (or parent-dir "") short-proj))
         ;; use `dir` if it is shorter than `long-proj` or `short-proj`
         (short-proj (if (>= (length short-proj) (length dir))
                         dir
                       short-proj))
         (long-proj (if (>= (length long-proj) (length dir))
                        dir
                      long-proj))
         (port (or (plist-get params :port) ""))
         (host (or (plist-get params :host) "localhost"))
         (remote-host (if (member host '("localhost" "127.0.0.1"))
                          ""
                        host))
         (repl-type (or (plist-get params :repl-type) "unknown"))
         (cljs-repl-type (or (and (eq repl-type 'cljs)
                                  (plist-get params :cljs-repl-type))
                             ""))
         (specs `((?h . ,host)
                  (?H . ,remote-host)
                  (?p . ,port)
                  (?j . ,short-proj)
                  (?J . ,long-proj)
                  (?r . ,repl-type)
                  (?S . ,cljs-repl-type)))
         (ses-name (or (plist-get params :session-name)
                       (format-spec cider-session-name-template specs)))
         (specs (append `((?s . ,ses-name)) specs))
         (specs (mapcar #'cider--ensure-spec-is-not-invokable specs)))
    (thread-last (format-spec template specs)
                 ;; remove extraneous separators
                 (replace-regexp-in-string "\\([:-]\\)[:-]+" "\\1")
                 (replace-regexp-in-string "\\(^[:-]\\)\\|\\([:-]$\\)" "")
                 (replace-regexp-in-string "[:-]\\([])*]\\)" "\\1"))))

(defun cider-make-session-name (params)
  "Create new session name given plist of connection PARAMS.
Session name can be customized with `cider-session-name-template'."
  (let* ((root-name (cider-format-connection-params cider-session-name-template params))
         (other-names (mapcar #'car (sesman-sessions 'CIDER)))
         (name root-name)
         (i 2))
    (while (member name other-names)
      (setq name (concat root-name "#" (number-to-string i))
            i (+ i 1)))
    name))


;;; REPL Buffer Init

(defvar-local cider-cljs-repl-type nil
  "The type of the ClojureScript runtime (\\='browser, \\='node, \\='figwheel, etc.).")

(defvar-local cider-repl-type nil
  "The type of this REPL buffer, usually either clj or cljs.")

(defvar-local cider-repl-cljs-upgrade-pending nil
  "Is the cljs repl currently pending?")

(defun cider-repl-type (repl-buffer)
  "Get REPL-BUFFER's type."
  (buffer-local-value 'cider-repl-type repl-buffer))

(defun cider-cljs-pending-p (repl-buffer)
  "Returns non nil when REPL-BUFFER is currently a pending cljs repl."
  (buffer-local-value 'cider-repl-cljs-upgrade-pending repl-buffer))

(defun cider-repl-type-for-buffer (&optional buffer)
  "Return the matching connection type (clj or cljs) for BUFFER.
BUFFER defaults to the `current-buffer'.  In cljc buffers return
multi.  This function infers connection type based on the major mode.
For the REPL type use the function `cider-repl-type'."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ((cider-clojurescript-major-mode-p) 'cljs)
     ((cider-clojurec-major-mode-p) cider-clojurec-eval-destination)
     ((cider-clojure-major-mode-p) 'clj)
     (cider-repl-type))))

(defun cider-set-repl-type (&optional type)
  "Set REPL TYPE to clj or cljs.
Assume that the current buffer is a REPL."
  (interactive)
  (let ((type (cider-maybe-intern (or type (completing-read
                                            (format "Set REPL type (currently `%s') to: "
                                                    cider-repl-type)
                                            '(clj cljs))))))
    (when (or (not (equal cider-repl-type type))
              (null mode-name))
      (setq cider-repl-type type)
      (setq mode-name (format "REPL[%s]" type))
      (let ((params (cider--gather-connect-params)))
        ;; We need to set current name to something else temporarily to avoid
        ;; false name duplication in `nrepl-repl-buffer-name`.
        (rename-buffer (generate-new-buffer-name "*dummy-cider-repl-buffer*"))
        (rename-buffer (nrepl-repl-buffer-name params))
        (when (and nrepl-log-messages nrepl-messages-buffer)
          (with-current-buffer nrepl-messages-buffer
            (rename-buffer (nrepl-messages-buffer-name params))))))))

(defun cider--choose-reusable-repl-buffer (params)
  "Find connection-less REPL buffer and ask the user for confirmation.
Return nil if no such buffers exists or the user has chosen not to reuse
the buffer.  If multiple dead REPLs exist, ask the user to choose one.
PARAMS is a plist as received by `cider-repl-create'."
  (when-let* ((repls (seq-filter (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'cider-repl-mode)
                                          (not (process-live-p (get-buffer-process b))))))
                                 (buffer-list))))
    (let* ((proj-dir (plist-get params :project-dir))
           (host (plist-get params :host))
           (port (plist-get params :port))
           (type (plist-get params :repl-type))
           (scored-repls
            (mapcar (lambda (b)
                      (let ((bparams (ignore-errors (cider--gather-connect-params nil b))))
                        (when (eq type (plist-get bparams :repl-type))
                          (cons b (+
                                   (if (equal proj-dir (plist-get bparams :project-dir)) 8 0)
                                   (if (equal host (plist-get bparams :host)) 4 0)
                                   (if (equal port (plist-get bparams :port)) 2 0))))))
                    repls))
           (sorted-repls (mapcar #'car (seq-sort-by #'cdr #'> (delq nil scored-repls)))))
      (cond ((null sorted-repls) nil)
            ((and (= 1 (length sorted-repls))
                  (eq cider-reuse-dead-repls 'prompt))
             (if (y-or-n-p (format "A dead REPL %s exists.  Reuse buffer? " (car sorted-repls)))
                 (car sorted-repls)
               (and (y-or-n-p "Kill dead REPL buffer?")
                    (kill-buffer (car sorted-repls))
                    nil)))
            ((and (< 1 (length sorted-repls))
                  (memq cider-reuse-dead-repls '(prompt auto)))
             (if (y-or-n-p "Dead REPL buffers exist.  Select one to reuse? ")
                 (get-buffer (completing-read "REPL buffer to reuse: " (mapcar #'buffer-name sorted-repls)
                                              nil t nil nil (car sorted-repls)))
               (and (y-or-n-p "Kill all dead REPL buffers?")
                    (mapc #'kill-buffer sorted-repls)
                    nil)))
            (cider-reuse-dead-repls ;; fallthrough for 'auto / 'any / other non-nil values
             (car sorted-repls))))))

(declare-function cider-default-err-handler "cider-eval")
(declare-function cider-repl-mode "cider-repl")
(declare-function cider-repl--state-handler "cider-repl")
(declare-function cider-repl-reset-markers "cider-repl")
(defvar-local cider-session-name nil)
(defvar-local cider-repl-init-function nil)
(defvar-local cider-launch-params nil)
(defun cider-repl-create (params)
  "Create new repl buffer.
PARAMS is a plist which contains :repl-type, :host, :port, :project-dir,
:repl-init-function and :session-name.  When non-nil, :repl-init-function
must be a function with no arguments which is called after repl creation
function with the repl buffer set as current."
  ;; Connection might not have been set as yet. Please don't send requests in
  ;; this function, but use cider--connected-handler instead.
  (let ((buffer (or (plist-get params :repl-buffer)
                    (and cider-reuse-dead-repls
                         (cider--choose-reusable-repl-buffer params))
                    (get-buffer-create (generate-new-buffer-name "*cider-uninitialized-repl*"))))
        (ses-name (or (plist-get params :session-name)
                      (cider-make-session-name params))))
    (with-current-buffer buffer
      (setq-local sesman-system 'CIDER)
      (setq-local default-directory (or (plist-get params :project-dir) default-directory))
      ;; creates a new session if session with ses-name doesn't already exist
      (sesman-add-object 'CIDER ses-name buffer 'allow-new)
      (unless (derived-mode-p 'cider-repl-mode)
        (cider-repl-mode))
      (setq nrepl-err-handler #'cider-default-err-handler
            ;; used as a new-repl marker in cider-set-repl-type
            mode-name nil
            cider-session-name ses-name
            nrepl-project-dir (plist-get params :project-dir)
            ;; Cljs repls are pending until they are upgraded. See cider-repl--state-handler
            cider-repl-type (plist-get params :repl-type)
            cider-repl-cljs-upgrade-pending (plist-get params :cider-repl-cljs-upgrade-pending)
            ;; ran at the end of cider--connected-handler
            cider-repl-init-function (plist-get params :repl-init-function)
            cider-launch-params params)
      (when-let ((type (plist-get params :cljs-repl-type)))
        (setq cider-cljs-repl-type type))
      (cider-repl-reset-markers)
      (add-hook 'nrepl-response-handler-functions #'cider-repl--state-handler nil 'local)
      (add-hook 'nrepl-connected-hook #'cider--connected-handler nil 'local)
      (add-hook 'nrepl-disconnected-hook #'cider--disconnected-handler nil 'local)
      (current-buffer))))


;;; Current/other REPLs

(defun cider--no-repls-user-error (type)
  "Throw \"No REPL\" user error customized for TYPE."
  (let ((type (cond
               ((or (eq type 'multi) (eq type 'any))
                "clj or cljs")
               ((listp type)
                (mapconcat #'identity type " or "))
               (type))))
    (user-error "No %s REPLs in current session \"%s\""
                type (car (sesman-current-session 'CIDER)))))

(defvar-local cider--ancillary-buffer-repl nil
  "Special buffer-local variable that contains reference to the REPL connection.
This should be set in ancillary CIDER buffers that originate from some
event (e.g. *cider-inspector*, *cider-error*) and which never change the
REPL (connection) which produced them.")

(defun cider-current-repl (&optional type ensure)
  "Get the most recent REPL of TYPE from the current session.
TYPE is either clj, cljs, multi, infer or any.
When infer or nil, infer the type from the current buffer.
If ENSURE is non-nil, throw an error if either there is
no linked session or there is no REPL of TYPE within the current session."
  (let ((type (cider-maybe-intern type)))
    (if (and (derived-mode-p 'cider-repl-mode)
             (or (null type)
                 (eq 'any type)
                 (eq 'infer type)
                 (eq cider-repl-type type)))
        ;; shortcut when in REPL buffer
        (current-buffer)
      (or cider--ancillary-buffer-repl
          (let* ((type (if (or (null type)
                               (eq 'infer type))
                           (cider-repl-type-for-buffer)
                         type))
                 (repls (cider-repls type ensure))
                 (repl (if (<= (length repls) 1)
                           (car repls)
                         ;; pick the most recent one
                         (seq-find (lambda (b)
                                     (member b repls))
                                   (buffer-list)))))
            (if (and ensure (null repl))
                (cider--no-repls-user-error type)
              repl))))))

(defun cider--match-repl-type (type buffer)
  "Return non-nil if TYPE matches BUFFER's REPL type."
  (let ((buffer-repl-type (cider-repl-type buffer)))
    (cond ((null buffer-repl-type) nil)
          ((or (null type) (eq type 'multi) (eq type 'any)) t)
          ((listp type) (member buffer-repl-type type))
          (t
           (or (string= type buffer-repl-type)
               (let ((capabilities
                      (buffer-local-value 'cider-connection-capabilities buffer)))
                 (cond ((listp type)
                        (cl-some (lambda (it) (member it capabilities)) type))
                       (t (member type capabilities)))))))))

(defun cider--get-host-from-session (session)
  "Returns the host associated with SESSION."
  (plist-get (cider--gather-session-params session)
             :host))

(defun cider--make-sessions-list-with-hosts (sessions)
  "Makes a list of SESSIONS and their hosts.
Returns a list of the form ((session1 host1) (session2 host2) ...)."
  (mapcar (lambda (session)
            (list session (cider--get-host-from-session session)))
          sessions))

(defun cider--get-sessions-with-same-host (session sessions)
  "Returns a list of SESSIONS with the same host as SESSION."
  (mapcar #'car
          (seq-filter (lambda (x)
                        (string-equal (cadr x)
                                      (cider--get-host-from-session session)))
                      (cider--make-sessions-list-with-hosts sessions))))

(defun cider--extract-connections (sessions)
  "Returns a flattened list of all session buffers in SESSIONS."
  (cl-reduce (lambda (x y)
               (append x (cdr y)))
             sessions
             :initial-value '()))

(defun cider-repls (&optional type ensure)
  "Return cider REPLs of TYPE from the current session.
If TYPE is nil or multi, return all REPLs.  If TYPE is a list of types,
return only REPLs of type contained in the list.  If ENSURE is non-nil,
throw an error if no linked session exists."
  (let ((type (cond
               ((listp type)
                (mapcar #'cider-maybe-intern type))
               ((cider-maybe-intern type))))
        (repls (pcase cider-merge-sessions
                 ('host
                  (if ensure
                      (or (cider--extract-connections (cider--get-sessions-with-same-host
                                                       (sesman-current-session 'CIDER)
                                                       (sesman-current-sessions 'CIDER)))
                          (user-error "No linked %s sessions" 'CIDER))
                    (cider--extract-connections (cider--get-sessions-with-same-host
                                                 (sesman-current-session 'CIDER)
                                                 (sesman-current-sessions 'CIDER)))))
                 ('project
                  (if ensure
                      (or (cider--extract-connections (sesman-current-sessions 'CIDER))
                          (user-error "No linked %s sessions" 'CIDER))
                    (cider--extract-connections (sesman-current-sessions 'CIDER))))
                 (_ (cdr (if ensure
                             (sesman-ensure-session 'CIDER)
                           (sesman-current-session 'CIDER)))))))
    (or (seq-filter (lambda (b)
                      (unless
                          (cider-cljs-pending-p b)
                        (cider--match-repl-type type b)))
                    repls)
        (when ensure
          (cider--no-repls-user-error type)))))

(defun cider-map-repls (which function)
  "Call FUNCTION once for each appropriate REPL as indicated by WHICH.
The function is called with one argument, the REPL buffer.  The appropriate
connections are found by inspecting the current buffer.  WHICH is one of
the following keywords:
 :auto - Act on the connections whose type matches the current buffer.  In
     `cljc' files, mapping happens over both types of REPLs.
 :clj (:cljs) - Map over clj (cljs)) REPLs only.
 :clj-strict (:cljs-strict) - Map over clj (cljs) REPLs but signal a
      `user-error' in `clojurescript-mode' (`clojure-mode').  Use this for
      commands only supported in Clojure (ClojureScript).
Error is signaled if no REPL buffers of specified type exist in current
session."
  (declare (indent 1))
  (let ((cur-type (cider-repl-type-for-buffer)))
    (cl-case which
      (:clj-strict (when (eq cur-type 'cljs)
                     (user-error "Clojure-only operation requested in a ClojureScript buffer")))
      (:cljs-strict (when (eq cur-type 'clj)
                      (user-error "ClojureScript-only operation requested in a Clojure buffer"))))
    (let* ((type (cl-case which
                   ((:clj :clj-strict) 'clj)
                   ((:cljs :cljs-strict) 'cljs)
                   (:auto (if (eq cur-type 'multi)
                              '(clj cljs)
                            cur-type))))
           (ensure (cl-case which
                     (:auto nil)
                     (t 'ensure)))
           (repls (cider-repls type ensure)))
      (mapcar function repls))))

;; REPLs double as connections in CIDER, so it's useful to be able to refer to
;; them as connections in certain contexts.
(defalias 'cider-current-connection #'cider-current-repl)
(defalias 'cider-connections #'cider-repls)
(defalias 'cider-map-connections #'cider-map-repls)
(defalias 'cider-connection-type-for-buffer #'cider-repl-type-for-buffer)

;; Deprecated after #2324 (introduction of sesman)

(define-obsolete-function-alias 'cider-current-repl-buffer #'cider-current-repl "0.18")
(define-obsolete-function-alias 'cider-repl-buffers #'cider-repls "0.18")
(define-obsolete-function-alias 'cider-current-session #'cider-nrepl-eval-session "0.18")
(define-obsolete-function-alias 'cider-current-tooling-session #'cider-nrepl-tooling-session "0.18")
(define-obsolete-function-alias 'nrepl-connection-buffer-name #'nrepl-repl-buffer-name "0.18")

(provide 'cider-connection)

;;; cider-connection.el ends here
