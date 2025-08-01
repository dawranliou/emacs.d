;;; magit-section.el --- Sections for read-only buffers  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; Homepage: https://github.com/magit/magit
;; Keywords: tools

;; Package-Version: 20250725.1036
;; Package-Revision: 3dc5669fdf8c
;; Package-Requires: (
;;     (emacs "28.1")
;;     (compat "30.1")
;;     (llama "1.0.0")
;;     (seq "2.24"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;; You should have received a copy of the AUTHORS.md file, which
;; lists all contributors.  If not, see https://magit.vc/authors.

;;; Commentary:

;; This package implements the main user interface of Magit — the
;; collapsible sections that make up its buffers.  This package used
;; to be distributed as part of Magit but now it can also be used by
;; other packages that have nothing to do with Magit or Git.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eieio)
(require 'llama)
(require 'subr-x)

;; For older Emacs releases we depend on an updated `seq' release from GNU
;; ELPA, for `seq-keep'.  Unfortunately something else may require `seq'
;; before `package' had a chance to put this version on the `load-path'.
(when (and (featurep 'seq)
           (not (fboundp 'seq-keep)))
  (unload-feature 'seq 'force))
(require 'seq)
;; Furthermore, by default `package' just silently refuses to upgrade.
(defconst magit--core-upgrade-instructions "\
Magit requires `%s' >= %s,
but due to bad defaults, Emacs' package manager, refuses to
upgrade this and other built-in packages to higher releases
from GNU Elpa.

To fix this, you have to add this to your init file:

  (setq package-install-upgrade-built-in t)

Then evaluate that expression by placing the cursor after it
and typing \\[eval-last-sexp].

Once you have done that, you have to explicitly upgrade `%s':

  \\[package-install] %s \\`RET'

Then you also must make sure the updated version is loaded,
by evaluating this form:

  (progn (unload-feature \\='%s t) (require \\='%s))

If this does not work, then try uninstalling Magit and all of its
dependencies.  After that exit and restart Emacs, and only then
reinstalling Magit.

If you don't use the `package' package manager but still get
this warning, then your chosen package manager likely has a
similar defect.")
(unless (fboundp 'seq-keep)
  (display-warning 'magit (substitute-command-keys
                           (format magit--core-upgrade-instructions
                                   'seq "2.24" 'seq 'seq 'seq 'seq))
                   :emergency))

(require 'cursor-sensor)
(require 'format-spec)

(eval-when-compile (require 'benchmark))

;; For `magit-section-get-relative-position'
(declare-function magit-hunk-section-p "magit-diff" (section) t)

(define-obsolete-variable-alias 'magit-keep-region-overlay
  'magit-section-keep-region-overlay "Magit-Section 4.0.0")

;;; Hooks

(defvar magit-section-movement-hook nil
  "Hook run by `magit-section-goto'.
That function in turn is used by all section movement commands.
See also info node `(magit)Section Movement'.")

(defvar magit-section-set-visibility-hook
  (list #'magit-section-cached-visibility)
  "Hook used to set the initial visibility of a section.
Stop at the first function that returns non-nil.  The returned
value should be `show', `hide' or nil.  If no function returns
non-nil, determine the visibility as usual, i.e., use the
hardcoded section specific default (see `magit-insert-section').")

;;; Options

(defgroup magit-section nil
  "Expandable sections."
  :link '(info-link "(magit)Sections")
  :group 'extensions)

(defcustom magit-section-highlight-current t
  "Whether to highlight the current section."
  :package-version '(magit-section . "4.3.6")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-highlight-selection t
  "Whether to highlight the selected sections.
If you disable this, you probably also want to disable
`magit-section-highlight-current' to get the region to
always look as it would be in non-magit buffers."
  :package-version '(magit-section . "4.3.6")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-show-child-count t
  "Whether to append the number of children to section headings.
This only applies to sections for which doing so makes sense."
  :package-version '(magit-section . "2.1.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-cache-visibility t
  "Whether to cache visibility of sections.

Sections always retain their visibility state when they are being
recreated during a refresh.  But if a section disappears and then
later reappears again, then this option controls whether this is
the case.

If t, then cache the visibility of all sections.  If a list of
section types, then only do so for matching sections.  If nil,
then don't do so for any sections."
  :package-version '(magit-section . "2.12.0")
  :group 'magit-section
  :type '(choice (const  :tag "Don't cache visibility" nil)
                 (const  :tag "Cache visibility of all sections" t)
                 (repeat :tag "Cache visibility for section types" symbol)))

(defcustom magit-section-initial-visibility-alist
  '((stashes . hide))
  "Alist controlling the initial visibility of sections.

Each element maps a section type or lineage to the initial
visibility state for such sections.  The state has to be one of
`show' or `hide', or a function that returns one of these symbols.
A function is called with the section as the only argument.

Use the command `magit-describe-section' to determine a section's
lineage or type.  The vector in the output is the section lineage
and the type is the first element of that vector.  Wildcards can
be used, see `magit-section-match'.

Currently this option is only used to override hardcoded defaults,
but in the future it will also be used set the defaults.

An entry whose key is `magit-status-initial-section' specifies
the visibility of the section `magit-status-goto-initial-section'
jumps to.  This does not only override defaults, but also other
entries of this alist."
  :package-version '(magit-section . "2.12.0")
  :group 'magit-section
  :type '(alist :key-type (sexp :tag "Section type/lineage")
                :value-type (choice (const hide)
                                    (const show)
                                    function)))

(defcustom magit-section-visibility-indicator
  (if (window-system)
      '(magit-fringe-bitmap> . magit-fringe-bitmapv)
    (cons (if (char-displayable-p ?…) "…" "...")
          t))
  "Whether and how to indicate that a section can be expanded/collapsed.

If nil, then don't show any indicators.
Otherwise the value has to have one of these two forms:

\(EXPANDABLE-BITMAP . COLLAPSIBLE-BITMAP)

  Both values have to be variables whose values are fringe
  bitmaps.  In this case every section that can be expanded or
  collapsed gets an indicator in the left fringe.

  To provide extra padding around the indicator, set
  `left-fringe-width' in `magit-mode-hook'.

\(STRING . BOOLEAN)

  In this case STRING (usually an ellipsis) is shown at the end
  of the heading of every collapsed section.  Expanded sections
  get no indicator.  The cdr controls whether the appearance of
  these ellipsis take section highlighting into account.  Doing
  so might potentially have an impact on performance, while not
  doing so is kinda ugly."
  :package-version '(magit-section . "3.0.0")
  :group 'magit-section
  :type '(choice (const :tag "No indicators" nil)
                 (cons  :tag "Use +- fringe indicators"
                        (const magit-fringe-bitmap+)
                        (const magit-fringe-bitmap-))
                 (cons  :tag "Use >v fringe indicators"
                        (const magit-fringe-bitmap>)
                        (const magit-fringe-bitmapv))
                 (cons  :tag "Use bold >v fringe indicators)"
                        (const magit-fringe-bitmap-bold>)
                        (const magit-fringe-bitmap-boldv))
                 (cons  :tag "Use custom fringe indicators"
                        (variable :tag "Expandable bitmap variable")
                        (variable :tag "Collapsible bitmap variable"))
                 (cons  :tag "Use ellipses at end of headings"
                        (string :tag "Ellipsis" "…")
                        (choice :tag "Use face kludge"
                                (const :tag "Yes (potentially slow)" t)
                                (const :tag "No (kinda ugly)" nil)))))

(defcustom magit-section-keep-region-overlay nil
  "Whether to keep the region overlay when there is a valid selection.

We strongly suggest that you keep the default value, nil.

By default Magit removes the regular region overlay if, and only
if, that region constitutes a valid selection as understood by
Magit commands.  Otherwise it does not remove that overlay, and
the region looks like it would in other buffers.

There are two types of such valid selections: hunk-internal
regions and regions that select two or more sibling sections.
In such cases Magit removes the region overlay and instead
highlights a slightly larger range.  All text (for hunk-internal
regions) or the headings of all sections (for sibling selections)
that are inside that range (not just inside the region) are acted
on by commands such as the staging command.  This buffer range
begins at the beginning of the line on which the region begins
and ends at the end of the line on which the region ends.

Because Magit acts on this larger range and not the region, it is
actually quite important to visualize that larger range.  If we
don't do that, then one might think that these commands act on
the region instead.  If you want to *also* visualize the region,
then set this option to t.  But please note that when the region
does *not* constitute a valid selection, then the region is
*always* visualized as usual, and that it is usually under such
circumstances that you want to use a non-magit command to act on
the region.

Depending on the used theme, the `magit-*-highlight-selection'
faces might conflict with the `region' face.  If that happens and
it bothers you, then you have to customize these faces to address
the conflicts."
  :package-version '(magit-section . "2.3.0")
  :group 'magit-section
  :type 'boolean)

(defcustom magit-section-disable-line-numbers t
  "In Magit buffers, whether to disable modes that display line numbers.

Some users who turn on `global-display-line-numbers-mode' (or
`global-nlinum-mode' or `global-linum-mode') expect line numbers
to be displayed everywhere except in Magit buffers.  Other users
do not expect Magit buffers to be treated differently.  At least
in theory users in the first group should not use the global mode,
but that ship has sailed, thus this option."
  :package-version '(magit-section . "3.0.0")
  :group 'magit-section
  :type 'boolean)

;;; Variables

(defvar-local magit-section-preserve-visibility t)

(defvar-local magit-section-pre-command-region-p nil)
(defvar-local magit-section-pre-command-section nil)

(defvar-local magit-section-highlight-force-update nil)
(defvar-local magit-section-highlight-overlays nil)
(defvar-local magit-section-selection-overlays nil)
(defvar-local magit-section-highlighted-sections nil
  "List of highlighted sections that may have to be repainted on focus change.")
(defvar-local magit-section-focused-sections nil)

(defvar-local magit-section-inhibit-markers nil)
(defvar-local magit-section-insert-in-reverse nil)

(defvar-local magit--refreshing-buffer-p nil
  "Whether the current buffer is presently being refreshed.")

;;; Faces

(defgroup magit-section-faces nil
  "Faces used by Magit-Section."
  :group 'magit-section
  :group 'faces)

(defface magit-section-highlight
  '((((class color) (background light))
     :extend t
     :background "grey95")
    (((class color) (background  dark))
     :extend t
     :background "grey20"))
  "Face for highlighting the current section."
  :group 'magit-section-faces)

(defface magit-section-heading
  '((((class color) (background light))
     :extend t
     :foreground "DarkGoldenrod4"
     :weight bold)
    (((class color) (background  dark))
     :extend t
     :foreground "LightGoldenrod2"
     :weight bold))
  "Face for section headings."
  :group 'magit-section-faces)

(defface magit-section-secondary-heading
  '((t :extend t :weight bold))
  "Face for section headings of some secondary headings."
  :group 'magit-section-faces)

(defface magit-section-heading-selection
  '((((class color) (background light))
     :extend t
     :foreground "salmon4")
    (((class color) (background  dark))
     :extend t
     :foreground "LightSalmon3"))
  "Face for selected section headings."
  :group 'magit-section-faces)

(defface magit-section-child-count '((t nil))
  "Face used for child counts at the end of some section headings."
  :group 'magit-section-faces)

;;; Classes

(defvar magit--current-section-hook nil
  "Internal variable used for `magit-describe-section'.")

(defvar magit--section-type-alist nil)

(defclass magit-section ()
  ((type     :initform nil :initarg :type)
   (keymap   :initform nil)
   (value    :initform nil)
   (start    :initform nil)
   (content  :initform nil)
   (end      :initform nil)
   (hidden)
   (painted)
   (washer   :initform nil :initarg :washer)
   (inserter :initform (symbol-value 'magit--current-section-hook))
   (selective-highlight    :initform nil :initarg :selective-highlight)
   (heading-highlight-face :initform nil :initarg :heading-highlight-face)
   (heading-selection-face :initform nil :initarg :heading-selection-face)
   (parent   :initform nil)
   (children :initform nil)))

;;; Mode

(defvar symbol-overlay-inhibit-map)

(defvar-keymap magit-section-heading-map
  :doc "Keymap used in the heading line of all expandable sections.
This keymap is used in addition to the section-specific keymap, if any."
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'magit-mouse-toggle-section
  "<double-mouse-2>" #'magit-mouse-toggle-section)

(defvar-keymap magit-section-mode-map
  :doc "Parent keymap for keymaps of modes derived from `magit-section-mode'."
  :full t
  :suppress t
  "<left-fringe> <mouse-1>" #'magit-mouse-toggle-section
  "<left-fringe> <mouse-2>" #'magit-mouse-toggle-section
  "TAB"       #'magit-section-toggle
  "C-c TAB"   #'magit-section-cycle
  "C-<tab>"   #'magit-section-cycle
  "M-<tab>"   #'magit-section-cycle
  ;; <backtab> is the most portable binding for Shift+Tab.
  "<backtab>" #'magit-section-cycle-global
  "^"   #'magit-section-up
  "p"   #'magit-section-backward
  "n"   #'magit-section-forward
  "M-p" #'magit-section-backward-sibling
  "M-n" #'magit-section-forward-sibling
  "1"   #'magit-section-show-level-1
  "2"   #'magit-section-show-level-2
  "3"   #'magit-section-show-level-3
  "4"   #'magit-section-show-level-4
  "M-1" #'magit-section-show-level-1-all
  "M-2" #'magit-section-show-level-2-all
  "M-3" #'magit-section-show-level-3-all
  "M-4" #'magit-section-show-level-4-all)

(define-derived-mode magit-section-mode special-mode "Magit-Sections"
  "Parent major mode from which major modes with Magit-like sections inherit.

Magit-Section is documented in info node `(magit-section)'."
  :interactive nil
  :group 'magit-section
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  ;; Turn off syntactic font locking, but not by setting
  ;; `font-lock-defaults' because that would enable font locking, and
  ;; not all magit plugins may be ready for that (see #3950).
  (setq font-lock-keywords-only t)
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'pre-command-hook #'magit-section-pre-command-hook nil t)
  (add-hook 'post-command-hook #'magit-section-post-command-hook t t)
  (add-hook 'deactivate-mark-hook #'magit-section-deactivate-mark t t)
  (setq-local redisplay-highlight-region-function
              #'magit-section--highlight-region)
  (setq-local redisplay-unhighlight-region-function
              #'magit-section--unhighlight-region)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'magit-section--remove-text-properties)
  (when (fboundp 'magit-section-context-menu)
    (add-hook 'context-menu-functions #'magit-section-context-menu 10 t))
  (when magit-section-disable-line-numbers
    (when (and (fboundp 'linum-mode)
               (bound-and-true-p global-linum-mode))
      (linum-mode -1))
    (when (and (fboundp 'nlinum-mode)
               (bound-and-true-p global-nlinum-mode))
      (nlinum-mode -1))
    (when (and (fboundp 'display-line-numbers-mode)
               (bound-and-true-p global-display-line-numbers-mode))
      (display-line-numbers-mode -1)))
  (when (fboundp 'magit-preserve-section-visibility-cache)
    (add-hook 'kill-buffer-hook #'magit-preserve-section-visibility-cache)))

(defun magit-section--remove-text-properties (string)
  "Remove all text-properties from STRING.
Most importantly `magit-section'."
  (set-text-properties 0 (length string) nil string)
  string)

;;; Core

(defvar-local magit-root-section nil
  "The root section in the current buffer.
All other sections are descendants of this section.  The value
of this variable is set by `magit-insert-section' and you should
never modify it.")
(put 'magit-root-section 'permanent-local t)

(defvar-local magit--context-menu-section nil "For internal use only.")

(defvar magit--context-menu-buffer nil "For internal use only.")

(defun magit-point ()
  "Return point or the position where the context menu was invoked.
When using the context menu, return the position the user clicked
on, provided the current buffer is the buffer in which the click
occurred.  Otherwise return the same value as `point'."
  (if magit--context-menu-section
      (magit-menu-position)
    (point)))

(defun magit-thing-at-point (thing &optional no-properties)
  "Return the THING at point or where the context menu was invoked.
When using the context menu, return the thing the user clicked
on, provided the current buffer is the buffer in which the click
occurred.  Otherwise return the same value as `thing-at-point'.
For the meaning of THING and NO-PROPERTIES see that function."
  (if-let ((pos (magit-menu-position)))
      (save-excursion
        (goto-char pos)
        (thing-at-point thing no-properties))
    (thing-at-point thing no-properties)))

(defun magit-current-section ()
  "Return the section at point or where the context menu was invoked.
When using the context menu, return the section that the user
clicked on, provided the current buffer is the buffer in which
the click occurred.  Otherwise return the section at point."
  (or magit--context-menu-section
      (magit-section-at)
      magit-root-section))

(defun magit-section-at (&optional position)
  "Return the section at POSITION, defaulting to point."
  (get-text-property (or position (point)) 'magit-section))

(defun magit-section-ident (section)
  "Return an unique identifier for SECTION.
The return value has the form ((TYPE . VALUE)...)."
  (cons (cons (oref section type)
              (magit-section-ident-value section))
        (and-let* ((parent (oref section parent)))
          (magit-section-ident parent))))

(defun magit-section-equal (a b)
  "Return t if A an B are the same section."
  (and a b (equal (magit-section-ident a)
                  (magit-section-ident b))))

(cl-defgeneric magit-section-ident-value (object)
  "Return OBJECT's value, making it constant and unique if necessary.

This is used to correlate different incarnations of the same
section, see `magit-section-ident' and `magit-get-section'.

Sections whose values are not constant and/or unique should
implement a method that return a value that can be used for
thispurpose.")

(cl-defmethod magit-section-ident-value ((section magit-section))
  "Return the value unless it is an object.

Different object incarnations representing the same value tend to
not be equal, so call this generic function on the object itself
to determine a constant value."
  (let ((value (oref section value)))
    (if (eieio-object-p value)
        (magit-section-ident-value value)
      value)))

(cl-defmethod magit-section-ident-value ((object eieio-default-superclass))
  "For values that are objects, simply return the object itself.
Two objects that represent the same entity are not `equal'.  So if
the values of the objects of a certain section class are themselves
objects, then a method has to be defined for objects of one of the
involved classes."
  object)

(defun magit-get-section (ident &optional root)
  "Return the section identified by IDENT.
IDENT has to be a list as returned by `magit-section-ident'.
If optional ROOT is non-nil, then search in that section tree
instead of in the one whose root `magit-root-section' is."
  (setq ident (reverse ident))
  (let ((section (or root magit-root-section)))
    (when (eq (car (pop ident))
              (oref section type))
      (while (and ident
                  (pcase-let ((`(,type . ,value) (car ident)))
                    (setq section
                          (cl-find-if
                           (##and (eq (oref % type) type)
                                  (equal (magit-section-ident-value %) value))
                           (oref section children)))))
        (pop ident))
      section)))

(defun magit-section-lineage (section &optional raw)
  "Return the lineage of SECTION.
If optional RAW is non-nil, return a list of section objects, beginning
with SECTION, otherwise return a list of section types."
  (cons (if raw section (oref section type))
        (and-let* ((parent (oref section parent)))
          (magit-section-lineage parent raw))))

(defvar-local magit-insert-section--current nil "For internal use only.")
(defvar-local magit-insert-section--parent  nil "For internal use only.")
(defvar-local magit-insert-section--oldroot nil "For internal use only.")

;;; Menu

(defvar magit-menu-common-value nil "See function `magit-menu-common-value'.")
(defvar magit-menu--desc-values nil "For internal use only.")

(defun magit-section-context-menu (menu click)
  "Populate MENU with Magit-Section commands at CLICK."
  (when-let ((section (save-excursion
                        (unless (region-active-p)
                          (mouse-set-point click))
                        (magit-section-at))))
    (unless (region-active-p)
      (setq magit--context-menu-buffer (current-buffer))
      (if-let ((alt (save-excursion
                      (mouse-set-point click)
                      (run-hook-with-args-until-success
                       'magit-menu-alternative-section-hook section))))
          (setq magit--context-menu-section (setq section alt))
        (setq magit--context-menu-section section)
        (magit-section-update-highlight t)))
    (when (magit-section-content-p section)
      (keymap-set-after menu "<magit-section-toggle>"
        `(menu-item
          ,(if (oref section hidden) "Expand section" "Collapse section")
          magit-section-toggle))
      (when-let (((not (oref section hidden)))
                 (children (oref section children)))
        (when (seq-some #'magit-section-content-p children)
          (when (seq-some (##oref % hidden) children)
            (keymap-set-after menu "<magit-section-show-children>"
              `(menu-item "Expand children"
                          magit-section-show-children)))
          (when (seq-some (##not (oref % hidden)) children)
            (keymap-set-after menu "<magit-section-hide-children>"
              `(menu-item "Collapse children"
                          magit-section-hide-children)))))
      (keymap-set-after menu "<separator-magit-1>" menu-bar-separator))
    (keymap-set-after menu "<magit-describe-section>"
      `(menu-item "Describe section" magit-describe-section))
    (when-let ((map (oref section keymap)))
      (keymap-set-after menu "<separator-magit-2>" menu-bar-separator)
      (when (symbolp map)
        (setq map (symbol-value map)))
      (setq magit-menu-common-value (magit-menu-common-value section))
      (setq magit-menu--desc-values (magit-menu--desc-values section))
      (map-keymap (lambda (key binding)
                    (when (consp binding)
                      (define-key-after menu (vector key)
                        (copy-sequence binding))))
                  (menu-bar-keymap map))))
  menu)

(defun magit-menu-item (desc def &optional props)
  "Return a menu item named DESC binding DEF and using PROPS.

If DESC contains a supported %-spec, substitute the
expression (magit-menu-format-desc DESC) for that.
See `magit-menu-format-desc'."
  `(menu-item
    ,(if (and (stringp desc) (string-match-p "%[tTvsmMx]" desc))
         (list 'magit-menu-format-desc desc)
       desc)
    ,def
    ;; Without this, the keys for point would be shown instead
    ;; of the relevant ones from where the click occurred.
    :keys ,(##magit--menu-position-keys def)
    ,@props))

(defun magit--menu-position-keys (def)
  (or (ignore-errors
        (save-excursion
          (goto-char (magit-menu-position))
          (and-let* ((key (cl-find-if-not
                           (lambda (key)
                             (string-match-p "\\`<[0-9]+>\\'"
                                             (key-description key)))
                           (where-is-internal def))))
            (key-description key))))
      ""))

(defun magit-menu-position ()
  "Return the position where the context-menu was invoked.
If the current command wasn't invoked using the context-menu,
then return nil."
  (and magit--context-menu-section
       (ignore-errors
         (posn-point (event-start (aref (this-command-keys-vector) 0))))))

(defun magit-menu-highlight-point-section ()
  (setq magit-section-highlight-force-update t)
  (if (eq (current-buffer) magit--context-menu-buffer)
      (setq magit--context-menu-section nil)
    (if-let ((window (get-buffer-window magit--context-menu-buffer)))
        (with-selected-window window
          (setq magit--context-menu-section nil)
          (magit-section-update-highlight))
      (with-current-buffer magit--context-menu-buffer
        (setq magit--context-menu-section nil))))
  (setq magit--context-menu-buffer nil))

(defvar magit--plural-append-es '(branch))

(cl-defgeneric magit-menu-common-value (_section)
  "Return some value to be used by multiple menu items.
This function is called by `magit-section-context-menu', which
stores the value in `magit-menu-common-value'.  Individual menu
items can use it, e.g., in the expression used to set their
description."
  nil)

(defun magit-menu--desc-values (section)
  (let ((type (oref section type))
        (value (oref section value))
        (multiple (magit-region-sections nil t)))
    (list type
          value
          (format "%s %s" type value)
          (and multiple (length multiple))
          (if (memq type magit--plural-append-es) "es" "s"))))

(defun magit-menu-format-desc (format)
  "Format a string based on FORMAT and menu section or selection.
The following %-specs are allowed:
%t means \"TYPE\".
%T means \"TYPE\", or \"TYPEs\" if multiple sections are selected.
%v means \"VALUE\".
%s means \"TYPE VALUE\".
%m means \"TYPE VALUE\", or \"COUNT TYPEs\" if multiple sections
   are selected.
%M means \"VALUE\", or \"COUNT TYPEs\" if multiple sections are
   selected.
%x means the value of `magit-menu-common-value'."
  (pcase-let* ((`(,type ,value ,single ,count ,suffix) magit-menu--desc-values)
               (multiple (and count (format "%s %s%s" count type suffix))))
    (format-spec format
                 `((?t . ,type)
                   (?T . ,(format "%s%s" type (if count suffix "")))
                   (?v . ,value)
                   (?s . ,single)
                   (?m . ,(or multiple single))
                   (?M . ,(or multiple value))
                   (?x . ,(format "%s" magit-menu-common-value))))))

(define-advice context-menu-region (:around (fn menu click) magit-section-mode)
  "Disable in `magit-section-mode' buffers."
  (if (derived-mode-p 'magit-section-mode)
      menu
    (funcall fn menu click)))

;;; Commands
;;;; Movement

(defun magit-section-forward ()
  "Move to the beginning of the next visible section."
  (interactive)
  (if (eobp)
      (user-error "No next section")
    (let ((section (magit-current-section)))
      (if (oref section parent)
          (let ((next (and (not (oref section hidden))
                           (not (= (oref section end)
                                   (1+ (point))))
                           (car (oref section children)))))
            (while (and section (not next))
              (unless (setq next (car (magit-section-siblings section 'next)))
                (setq section (oref section parent))))
            (if next
                (magit-section-goto next)
              (user-error "No next section")))
        (magit-section-goto 1)))))

(defun magit-section-backward ()
  "Move to the beginning of the current or the previous visible section.
When point is at the beginning of a section then move to the
beginning of the previous visible section.  Otherwise move to
the beginning of the current section."
  (interactive)
  (if (bobp)
      (user-error "No previous section")
    (let ((section (magit-current-section)) children)
      (cond
       ((and (= (point)
                (1- (oref section end)))
             (setq children (oref section children)))
        (magit-section-goto (car (last children))))
       ((and (oref section parent)
             (not (= (point)
                     (oref section start))))
        (magit-section-goto section))
       (t
        (let ((prev (car (magit-section-siblings section 'prev))))
          (if prev
              (while (and (not (oref prev hidden))
                          (setq children (oref prev children)))
                (setq prev (car (last children))))
            (setq prev (oref section parent)))
          (cond (prev
                 (magit-section-goto prev))
                ((oref section parent)
                 (user-error "No previous section"))
                ;; Eob special cases.
                ((not (get-text-property (1- (point)) 'invisible))
                 (magit-section-goto -1))
                (t
                 (goto-char (previous-single-property-change
                             (1- (point)) 'invisible))
                 (forward-line -1)
                 (magit-section-goto (magit-current-section))))))))))

(defun magit-section-up ()
  "Move to the beginning of the parent section."
  (interactive)
  (if-let ((parent (oref (magit-current-section) parent)))
      (magit-section-goto parent)
    (user-error "No parent section")))

(defun magit-section-forward-sibling ()
  "Move to the beginning of the next sibling section.
If there is no next sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (oref current parent)
        (if-let ((next (car (magit-section-siblings current 'next))))
            (magit-section-goto next)
          (magit-section-forward))
      (magit-section-goto 1))))

(defun magit-section-backward-sibling ()
  "Move to the beginning of the previous sibling section.
If there is no previous sibling section, then move to the parent."
  (interactive)
  (let ((current (magit-current-section)))
    (if (oref current parent)
        (if-let ((previous (car (magit-section-siblings current 'prev))))
            (magit-section-goto previous)
          (magit-section-backward))
      (magit-section-goto -1))))

(defun magit-mouse-set-point (event &optional promote-to-region)
  "Like `mouse-set-point' but also call `magit-section-movement-hook'."
  (interactive "e\np")
  (mouse-set-point event promote-to-region)
  (run-hook-with-args 'magit-section-movement-hook (magit-current-section)))

(defun magit-section-goto (arg)
  "Run `magit-section-movement-hook'.
See info node `(magit)Section Movement'."
  (if (integerp arg)
      (progn (forward-line arg)
             (setq arg (magit-current-section)))
    (goto-char (oref arg start)))
  (run-hook-with-args 'magit-section-movement-hook arg))

(defun magit-section-set-window-start (section)
  "Ensure the beginning of SECTION is visible."
  (unless (pos-visible-in-window-p (oref section end))
    (set-window-start (selected-window) (oref section start))))

(defmacro magit-define-section-jumper
    (name heading type &optional value inserter &rest properties)
  "Define an interactive function to go to some section.
Together TYPE and VALUE identify the section.
HEADING is the displayed heading of the section."
  (declare (indent defun))
  `(transient-define-suffix ,name (&optional expand)
     ,(format "Jump to the section \"%s\".
With a prefix argument also expand it." heading)
     ,@properties
     ,@(and (not (plist-member properties :description))
            (list :description heading))
     ,@(and inserter
            `(:if (##memq ',inserter
                          (symbol-value
                           (intern (format "%s-sections-hook"
                                           (substring (symbol-name major-mode)
                                                      0 -5)))))))
     :inapt-if-not (##magit-get-section
                    (cons (cons ',type ,value)
                          (magit-section-ident magit-root-section)))
     (interactive "P")
     (if-let ((section (magit-get-section
                        (cons (cons ',type ,value)
                              (magit-section-ident magit-root-section)))))
         (progn (goto-char (oref section start))
                (when expand
                  (with-local-quit (magit-section-show section))
                  (recenter 0)))
       (message ,(format "Section \"%s\" wasn't found" heading)))))

;;;; Visibility

(defun magit-section-show (section)
  "Show the body of the current section."
  (interactive (list (magit-current-section)))
  (oset section hidden nil)
  (magit-section--opportunistic-wash section)
  (magit-section--opportunistic-paint section)
  (when-let ((beg (oref section content)))
    (remove-overlays beg (oref section end) 'invisible t))
  (magit-section-maybe-update-visibility-indicator section)
  (magit-section-maybe-cache-visibility section)
  (dolist (child (oref section children))
    (if (oref child hidden)
        (magit-section-hide child)
      (magit-section-show child))))

(defun magit-section-hide (section)
  "Hide the body of the current section."
  (interactive (list (magit-current-section)))
  (if (eq section magit-root-section)
      (user-error "Cannot hide root section")
    (oset section hidden t)
    (when-let ((beg (oref section content)))
      (let ((end (oref section end)))
        (when (< beg (point) end)
          (goto-char (oref section start)))
        (remove-overlays beg end 'invisible t)
        (let ((o (make-overlay beg end)))
          (overlay-put o 'evaporate t)
          (overlay-put o 'invisible t)
          (overlay-put o 'cursor-intangible t))))
    (magit-section-maybe-update-visibility-indicator section)
    (magit-section-maybe-cache-visibility section)))

(defun magit-section-toggle (section)
  "Toggle visibility of the body of the current section."
  (interactive (list (magit-current-section)))
  (cond ((eq section magit-root-section)
         (user-error "Cannot hide root section"))
        ((oref section hidden)
         (magit-section-show section))
        ((magit-section-hide section))))

(defun magit-section-toggle-children (section)
  "Toggle visibility of bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (let* ((children (oref section children))
         (show (seq-some (##oref % hidden) children)))
    (dolist (c children)
      (oset c hidden show)))
  (magit-section-show section))

(defun magit-section-show-children (section &optional depth)
  "Recursively show the bodies of children of the current section.
With a prefix argument show children that deep and hide deeper
children."
  (interactive (list (magit-current-section)))
  (magit-section-show-children-1 section depth)
  (magit-section-show section))

(defun magit-section-show-children-1 (section &optional depth)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (if depth
        (if (> depth 0)
            (magit-section-show-children-1 child (1- depth))
          (magit-section-hide child))
      (magit-section-show-children-1 child))))

(defun magit-section-hide-children (section)
  "Recursively hide the bodies of children of the current section."
  (interactive (list (magit-current-section)))
  (mapc #'magit-section-hide (oref section children)))

(defun magit-section-show-headings (section)
  "Recursively show headings of children of the current section.
Only show the headings, previously shown text-only bodies are
hidden."
  (interactive (list (magit-current-section)))
  (magit-section-show-headings-1 section)
  (magit-section-show section))

(defun magit-section-show-headings-1 (section)
  (dolist (child (oref section children))
    (oset child hidden nil)
    (when (or (oref child children)
              (not (oref child content)))
      (magit-section-show-headings-1 child))))

(defun magit-section-cycle (section)
  "Cycle visibility of current section and its children.

If this command is invoked using \\`C-<tab>' and that is globally bound
to `tab-next', then this command pivots to behave like that command, and
you must instead use \\`C-c TAB' to cycle section visibility.

If you would like to keep using \\`C-<tab>' to cycle section visibility
but also want to use `tab-bar-mode', then you have to prevent that mode
from using this key and instead bind another key to `tab-next'.  Because
`tab-bar-mode' does not use a mode map but instead manipulates the
global map, this involves advising `tab-bar--define-keys'."
  (interactive (list (magit-current-section)))
  (cond
   ((and (equal (this-command-keys) [C-tab])
         (eq (global-key-binding [C-tab]) 'tab-next)
         (fboundp 'tab-bar-switch-to-next-tab))
    (tab-bar-switch-to-next-tab current-prefix-arg))
   ((eq section magit-root-section)
    (magit-section-cycle-global))
   ((oref section hidden)
    (magit-section-show section)
    (magit-section-hide-children section))
   ((let ((children (oref section children)))
      (cond ((and (seq-some (##oref % hidden)   children)
                  (seq-some (##oref % children) children))
             (magit-section-show-headings section))
            ((seq-some #'magit-section-hidden-body children)
             (magit-section-show-children section))
            ((magit-section-hide section)))))))

(defun magit-section-cycle-global ()
  "Cycle visibility of all sections in the current buffer."
  (interactive)
  (let ((children (oref magit-root-section children)))
    (cond ((and (seq-some (##oref % hidden)   children)
                (seq-some (##oref % children) children))
           (magit-section-show-headings magit-root-section))
          ((seq-some #'magit-section-hidden-body children)
           (magit-section-show-children magit-root-section))
          (t
           (mapc #'magit-section-hide children)))))

(defun magit-section-hidden (section)
  "Return t if SECTION and/or an ancestor is hidden."
  (or (oref section hidden)
      (and-let* ((parent (oref section parent)))
        (magit-section-hidden parent))))

(defun magit-section-hidden-body (section &optional pred)
  "Return t if the content of SECTION or of any children is hidden."
  (if-let ((children (oref section children)))
      (funcall (or pred #'seq-some) #'magit-section-hidden-body children)
    (and (oref section content)
         (oref section hidden))))

(defun magit-section-content-p (section)
  "Return non-nil if SECTION has content or an unused washer function."
  (with-slots (content end washer) section
    (and content (or (not (= content end)) washer))))

(defun magit-section-invisible-p (section)
  "Return t if the SECTION's body is invisible.
When the body of an ancestor of SECTION is collapsed then
SECTION's body (and heading) obviously cannot be visible."
  (or (oref section hidden)
      (and-let* ((parent (oref section parent)))
        (magit-section-invisible-p parent))))

(defun magit-section-show-level (level)
  "Show surrounding sections up to LEVEL.
Likewise hide sections at higher levels.  If the region selects multiple
sibling sections, act on all marked trees.  If LEVEL is negative, show
all sections up to the absolute value of that, not just surrounding
sections."
  (if (< level 0)
      (let ((s (magit-current-section)))
        (setq level (- level))
        (while (> (1- (length (magit-section-ident s))) level)
          (setq s (oref s parent))
          (goto-char (oref s start)))
        (magit-section-show-children magit-root-section (1- level)))
    (dolist (section (or (magit-region-sections)
                         (list (magit-current-section))))
      (cl-do* ((s section
                  (oref s parent))
               (i (1- (length (magit-section-ident s)))
                  (cl-decf i)))
          ((cond ((< i level) (magit-section-show-children s (- level i 1)) t)
                 ((= i level) (magit-section-hide s) t))
           (magit-section-goto s))))))

(defun magit-section-show-level-1 ()
  "Show surrounding sections on first level."
  (interactive)
  (magit-section-show-level 1))

(defun magit-section-show-level-1-all ()
  "Show all sections on first level."
  (interactive)
  (magit-section-show-level -1))

(defun magit-section-show-level-2 ()
  "Show surrounding sections up to second level."
  (interactive)
  (magit-section-show-level 2))

(defun magit-section-show-level-2-all ()
  "Show all sections up to second level."
  (interactive)
  (magit-section-show-level -2))

(defun magit-section-show-level-3 ()
  "Show surrounding sections up to third level."
  (interactive)
  (magit-section-show-level 3))

(defun magit-section-show-level-3-all ()
  "Show all sections up to third level."
  (interactive)
  (magit-section-show-level -3))

(defun magit-section-show-level-4 ()
  "Show surrounding sections up to fourth level."
  (interactive)
  (magit-section-show-level 4))

(defun magit-section-show-level-4-all ()
  "Show all sections up to fourth level."
  (interactive)
  (magit-section-show-level -4))

(defun magit-mouse-toggle-section (event)
  "Toggle visibility of the clicked section.
Clicks outside either the section heading or the left fringe are
silently ignored."
  (interactive "e")
  (let* ((pos (event-start event))
         (section (magit-section-at (posn-point pos))))
    (if (eq (posn-area pos) 'left-fringe)
        (when section
          (while (not (magit-section-content-p section))
            (setq section (oref section parent)))
          (unless (eq section magit-root-section)
            (goto-char (oref section start))
            (magit-section-toggle section)))
      (magit-section-toggle section))))

;;;; Auxiliary

(defun magit-describe-section-briefly (&optional section ident interactive)
  "Show information about SECTION or the section at point.
With a prefix argument show the section identity instead of the
section lineage.  This command is intended for debugging purposes.
Non-interactively, just return the information.  Interactively,
or when INTERACTIVE is non-nil, show the section in the echo area."
  (interactive (list (magit-current-section) current-prefix-arg t))
  (unless section
    (setq section (magit-current-section)))
  (let ((str (format "#<%s %S %S %s-%s%s>"
                     (eieio-object-class section)
                     (let ((val (oref section value)))
                       (cond ((stringp val)
                              (substring-no-properties val))
                             ((and (eieio-object-p val)
                                   (fboundp 'cl-prin1-to-string))
                              (cl-prin1-to-string val))
                             (t
                              val)))
                     (if ident
                         (magit-section-ident section)
                       (apply #'vector (magit-section-lineage section)))
                     (and-let* ((m (oref section start)))
                       (if (markerp m) (marker-position m) m))
                     (if-let ((m (oref section content)))
                         (format "[%s-]"
                                 (if (markerp m) (marker-position m) m))
                       "")
                     (and-let* ((m (oref section end)))
                       (if (markerp m) (marker-position m) m)))))
    (when interactive
      (message "%s" str))
    str))

(cl-defmethod cl-print-object ((section magit-section) stream)
  "Print `magit-describe-section' result of SECTION."
  (princ (magit-describe-section-briefly section) stream))

(defun magit-describe-section (section &optional interactive-p)
  "Show information about the section at point."
  (interactive (list (magit-current-section) t))
  (let ((inserter-section section))
    (while (and inserter-section (not (oref inserter-section inserter)))
      (setq inserter-section (oref inserter-section parent)))
    (when (and inserter-section (oref inserter-section inserter))
      (setq section inserter-section)))
  (pcase (oref section inserter)
    (`((,hook ,fun) . ,src-src)
     (help-setup-xref `(magit-describe-section ,section) interactive-p)
     (with-help-window (help-buffer)
       (with-current-buffer standard-output
         (insert (format-message
                  "%s\n  is inserted by `%s'\n  from `%s'"
                  (magit-describe-section-briefly section)
                  (make-text-button (symbol-name fun) nil
                                    :type 'help-function
                                    'help-args (list fun))
                  (make-text-button (symbol-name hook) nil
                                    :type 'help-variable
                                    'help-args (list hook))))
         (pcase-dolist (`(,hook ,fun) src-src)
           (insert (format-message
                    ",\n  called by `%s'\n  from `%s'"
                    (make-text-button (symbol-name fun) nil
                                      :type 'help-function
                                      'help-args (list fun))
                    (make-text-button (symbol-name hook) nil
                                      :type 'help-variable
                                      'help-args (list hook)))))
         (insert ".\n\n")
         (insert
          (format-message
           "`%s' is "
           (make-text-button (symbol-name fun) nil
                             :type 'help-function 'help-args (list fun))))
         (describe-function-1 fun))))
    (_ (message "%s, inserter unknown"
                (magit-describe-section-briefly section)))))

;;; Match

(cl-defun magit-section-match
    (condition &optional (section (magit-current-section)))
  "Return t if SECTION matches CONDITION.

SECTION defaults to the section at point.  If SECTION is not
specified and there also is no section at point, then return
nil.

CONDITION can take the following forms:
  (CONDITION...)  matches if any of the CONDITIONs matches.
  [CLASS...]      matches if the section's class is the same
                  as the first CLASS or a subclass of that;
                  the section's parent class matches the
                  second CLASS; and so on.
  [* CLASS...]    matches sections that match [CLASS...] and
                  also recursively all their child sections.
  CLASS           matches if the section's class is the same
                  as CLASS or a subclass of that; regardless
                  of the classes of the parent sections.

Each CLASS should be a class symbol, identifying a class that
derives from `magit-section'.  For backward compatibility CLASS
can also be a \"type symbol\".  A section matches such a symbol
if the value of its `type' slot is `eq'.  If a type symbol has
an entry in `magit--section-type-alist', then a section also
matches that type if its class is a subclass of the class that
corresponds to the type as per that alist.

Note that it is not necessary to specify the complete section
lineage as printed by `magit-describe-section-briefly', unless
of course you want to be that precise."
  (and section (magit-section-match-1 condition section)))

(defun magit-section-match-1 (condition section)
  (cl-assert condition)
  (and section
       (if (listp condition)
           (seq-find (##magit-section-match-1 % section) condition)
         (magit-section-match-2 (if (symbolp condition)
                                    (list condition)
                                  (cl-coerce condition 'list))
                                section))))

(defun magit-section-match-2 (condition section)
  (if (eq (car condition) '*)
      (or (magit-section-match-2 (cdr condition) section)
          (and-let* ((parent (oref section parent)))
            (magit-section-match-2 condition parent)))
    (and (let ((c (car condition)))
           (if (class-p c)
               (cl-typep section c)
             (if-let ((class (cdr (assq c magit--section-type-alist))))
                 (cl-typep section class)
               (eq (oref section type) c))))
         (or (not (setq condition (cdr condition)))
             (and-let* ((parent (oref section parent)))
               (magit-section-match-2 condition parent))))))

(defun magit-section-value-if (condition &optional section)
  "If the section at point matches CONDITION, then return its value.

If optional SECTION is non-nil then test whether that matches
instead.  If there is no section at point and SECTION is nil,
then return nil.  If the section does not match, then return
nil.

See `magit-section-match' for the forms CONDITION can take."
  (and-let* ((section (or section (magit-current-section))))
    (and (magit-section-match condition section)
         (oref section value))))

(defmacro magit-section-case (&rest clauses)
  "Choose among clauses on the type of the section at point.

Each clause looks like (CONDITION BODY...).  The type of the
section is compared against each CONDITION; the BODY forms of the
first match are evaluated sequentially and the value of the last
form is returned.  Inside BODY the symbol `it' is bound to the
section at point.  If no clause succeeds or if there is no
section at point, return nil.

See `magit-section-match' for the forms CONDITION can take.
Additionally a CONDITION of t is allowed in the final clause, and
matches if no other CONDITION match, even if there is no section
at point."
  (declare (indent 0)
           (debug (&rest (sexp body))))
  `(let* ((it (magit-current-section)))
     (cond ,@(mapcar (lambda (clause)
                       `(,(or (eq (car clause) t)
                              `(and it
                                    (magit-section-match-1 ',(car clause) it)))
                         ,@(cdr clause)))
                     clauses))))

(defun magit-section-match-assoc (section alist)
  "Return the value associated with SECTION's type or lineage in ALIST."
  (seq-some (pcase-lambda (`(,key . ,val))
              (and (magit-section-match-1 key section) val))
            alist))

;;; Create

(defvar magit-insert-section-hook nil
  "Hook run after `magit-insert-section's BODY.
Avoid using this hook and only ever do so if you know
what you are doing and are sure there is no other way.")

(defmacro magit-insert-section (&rest args)
  "Insert a section at point.

Create a section object of type CLASS, storing VALUE in its
`value' slot, and insert the section at point.  CLASS is a
subclass of `magit-section' or has the form `(eval FORM)', in
which case FORM is evaluated at runtime and should return a
subclass.  In other places a sections class is often referred
to as its \"type\".

Many commands behave differently depending on the class of the
current section and sections of a certain class can have their
own keymap, which is specified using the `keymap' class slot.
The value of that slot should be a variable whose value is a
keymap.

For historic reasons Magit and Forge in most cases use symbols
as CLASS that don't actually identify a class and that lack the
appropriate package prefix.  This works due to some undocumented
kludges, which are not available to other packages.

When optional HIDE is non-nil collapse the section body by
default, i.e., when first creating the section, but not when
refreshing the buffer.  Else expand it by default.  This can be
overwritten using `magit-section-set-visibility-hook'.  When a
section is recreated during a refresh, then the visibility of
predecessor is inherited and HIDE is ignored (but the hook is
still honored).

BODY is any number of forms that actually insert the section's
heading and body.  Optional NAME, if specified, has to be a
symbol, which is then bound to the object of the section being
inserted.

Before BODY is evaluated the `start' of the section object is set
to the value of `point' and after BODY was evaluated its `end' is
set to the new value of `point'; BODY is responsible for moving
`point' forward.

If it turns out inside BODY that the section is empty, then
`magit-cancel-section' can be used to abort and remove all traces
of the partially inserted section.  This can happen when creating
a section by washing Git's output and Git didn't actually output
anything this time around.

\(fn [NAME] (CLASS &optional VALUE HIDE) &rest BODY)"
  (declare (indent 1) ;sic
           (debug ([&optional symbolp]
                   (&or [("eval" form) &optional form form &rest form]
                        [symbolp &optional form form &rest form])
                   body)))
  (pcase-let* ((bind (and (symbolp (car args))
                          (pop args)))
               (`((,class ,value ,hide . ,args) . ,body) args)
               (obj (gensym "section")))
    `(let* ((,obj (magit-insert-section--create
                   ,(if (eq (car-safe class) 'eval) (cadr class) `',class)
                   ,value ,hide ,@args))
            (magit-insert-section--current ,obj)
            (magit-insert-section--oldroot
             (or magit-insert-section--oldroot
                 (and (not magit-insert-section--parent)
                      (prog1 magit-root-section
                        (setq magit-root-section ,obj)))))
            (magit-insert-section--parent ,obj))
       (catch 'cancel-section
         ,@(if bind `((let ((,bind ,obj)) ,@body)) body)
         (magit-insert-section--finish ,obj))
       ,obj)))

(defun magit-insert-section--create (class value hide &rest args)
  (let (type)
    (if (class-p class)
        (setq type (or (car (rassq class magit--section-type-alist))
                       class))
      (setq type class)
      (setq class (or (cdr (assq class magit--section-type-alist))
                      'magit-section)))
    (let ((obj (apply class :type type args)))
      (oset obj value value)
      (oset obj parent magit-insert-section--parent)
      (oset obj start (if magit-section-inhibit-markers (point) (point-marker)))
      (unless (slot-boundp obj 'hidden)
        (oset obj hidden
              (let (set old)
                (cond
                 ((setq set (run-hook-with-args-until-success
                             'magit-section-set-visibility-hook obj))
                  (eq set 'hide))
                 ((setq old (and (not magit-section-preserve-visibility)
                                 magit-insert-section--oldroot
                                 (magit-get-section
                                  (magit-section-ident obj)
                                  magit-insert-section--oldroot)))
                  (oref old hidden))
                 ((setq set (magit-section-match-assoc
                             obj magit-section-initial-visibility-alist))
                  (eq (if (functionp set) (funcall set obj) set) 'hide))
                 (hide)))))
      (unless (oref obj keymap)
        (let ((type (oref obj type)))
          (oset obj keymap
                (or (let ((sym (intern (format "magit-%s-section-map" type))))
                      (and (boundp sym) sym))
                    (let ((sym (intern (format "forge-%s-section-map" type))))
                      (and (boundp sym) sym))))))
      obj)))

(defun magit-insert-section--finish (obj)
  (run-hooks 'magit-insert-section-hook)
  (if magit-section-inhibit-markers
      (oset obj end (point))
    (oset obj end (point-marker))
    (set-marker-insertion-type (oref obj start) t))
  (cond
   ((eq obj magit-root-section)
    (when (eq magit-section-inhibit-markers 'delay)
      (setq magit-section-inhibit-markers nil)
      (magit-map-sections
       (lambda (section)
         (oset section start (copy-marker (oref section start) t))
         (oset section end   (copy-marker (oref section end)   t))))))
   (t
    (magit-section--set-section-properties obj)
    (magit-section-maybe-add-heading-map obj)
    (when (oref obj children)
      (magit-insert-child-count obj))
    (if magit-section-insert-in-reverse
        (push obj (oref (oref obj parent) children))
      (let ((parent (oref obj parent)))
        (oset parent children
              (nconc (oref parent children)
                     (list obj)))))))
  (when magit-section-insert-in-reverse
    (oset obj children (nreverse (oref obj children)))))

(defun magit-cancel-section (&optional if-empty)
  "Cancel inserting the section that is currently being inserted.

Canceling returns from the inner most use of `magit-insert-section' and
removes all text that was inserted by that.

If optional IF-EMPTY is non-nil, then only cancel the section, if it is
empty.  If a section is split into a heading and a body (i.e., when its
`content' slot is non-nil), then only check if the body is empty."
  (when (and magit-insert-section--current
             (or (not if-empty)
                 (= (point) (or (oref magit-insert-section--current content)
                                (oref magit-insert-section--current start)))))
    (if (eq magit-insert-section--current magit-root-section)
        (insert "(empty)\n")
      (delete-region (oref magit-insert-section--current start)
                     (point))
      (setq magit-insert-section--current nil)
      (throw 'cancel-section nil))))

(defun magit-insert-heading (&rest args)
  "Insert the heading for the section currently being inserted.

This function should only be used inside `magit-insert-section'.

When called without any arguments, then just set the `content'
slot of the object representing the section being inserted to
a marker at `point'.  The section should only contain a single
line when this function is used like this.

When called with arguments ARGS, which have to be strings, or
nil, then insert those strings at point.  The section should not
contain any text before this happens and afterwards it should
again only contain a single line.  If the `face' property is set
anywhere inside any of these strings, then insert all of them
unchanged.  Otherwise use the `magit-section-heading' face for
all inserted text.

The `content' property of the section object is the end of the
heading (which lasts from `start' to `content') and the beginning
of the the body (which lasts from `content' to `end').  If the
value of `content' is nil, then the section has no heading and
its body cannot be collapsed.  If a section does have a heading,
then its height must be exactly one line, including a trailing
newline character.  This isn't enforced, you are responsible for
getting it right.  The only exception is that this function does
insert a newline character if necessary

If provided, optional CHILD-COUNT must evaluate to an integer or
boolean.  If t, then the count is determined once the children have been
inserted, using `magit-insert-child-count' (which see).  For historic
reasons, if the heading ends with \":\", the count is substituted for
that, at this time as well.  If `magit-section-show-child-count' is nil,
no counts are inserted

\n(fn [CHILD-COUNT] &rest STRINGS)"
  (declare (indent defun))
  (when args
    (let ((count (and (or (integerp (car args))
                          (booleanp (car args)))
                      (pop args)))
          (heading (apply #'concat args)))
      (insert (if (or (text-property-not-all 0 (length heading)
                                             'font-lock-face nil heading)
                      (text-property-not-all 0 (length heading)
                                             'face nil heading))
                  heading
                (propertize heading 'font-lock-face 'magit-section-heading)))
      (when (and count magit-section-show-child-count)
        (insert (propertize (format " (%s)" count)
                            'font-lock-face 'magit-section-child-count)))))
  (unless (bolp)
    (insert ?\n))
  (when (fboundp 'magit-maybe-make-margin-overlay)
    (magit-maybe-make-margin-overlay))
  (oset magit-insert-section--current content
        (if magit-section-inhibit-markers (point) (point-marker))))

(defmacro magit-insert-section-body (&rest body)
  "Use BODY to insert the section body, once the section is expanded.
If the section is expanded when it is created, then this is
like `progn'.  Otherwise BODY isn't evaluated until the section
is explicitly expanded."
  (declare (indent 0))
  (let ((f (gensym))
        (s (gensym))
        (l (gensym)))
    `(let ((,f (lambda () ,@body)))
       (if (oref magit-insert-section--current hidden)
           (oset magit-insert-section--current washer
                 (let ((,s magit-insert-section--current))
                   (lambda ()
                     (let ((,l (magit-section-lineage ,s t)))
                       (dolist (s ,l)
                         (set-marker-insertion-type (oref s end) t))
                       (funcall ,f)
                       (dolist (s ,l)
                         (set-marker-insertion-type (oref s end) nil))
                       (magit-section--set-section-properties ,s)
                       (magit-section-maybe-remove-heading-map ,s)
                       (magit-section-maybe-remove-visibility-indicator ,s)))))
         (funcall ,f)))))

(defun magit-insert-headers (hook)
  (let* ((header-sections nil)
         (fn (##push magit-insert-section--current header-sections)))
    (unwind-protect
        (progn
          (add-hook 'magit-insert-section-hook fn -90 t)
          (magit-run-section-hook hook)
          (when header-sections
            (insert "\n")
            ;; Make the first header into the parent of the rest.
            (when (cdr header-sections)
              (setq header-sections (nreverse header-sections))
              (let* ((1st-header (pop header-sections))
                     (header-parent (oref 1st-header parent)))
                (oset header-parent children (list 1st-header))
                (oset 1st-header children header-sections)
                (oset 1st-header content (oref (car header-sections) start))
                (oset 1st-header end (oref (car (last header-sections)) end))
                (dolist (sub-header header-sections)
                  (oset sub-header parent 1st-header))
                (magit-section-maybe-add-heading-map 1st-header)))))
      (remove-hook 'magit-insert-section-hook fn t))))

(defun magit-section--set-section-properties (section)
  (pcase-let* (((eieio start end children keymap) section)
               (props `( magit-section ,section
                         ,@(and-let* ((map (symbol-value keymap)))
                             (list 'keymap map)))))
    (if children
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (let ((next (or (next-single-property-change (point) 'magit-section)
                            end)))
              (unless (magit-section-at)
                (add-text-properties (point) next props))
              (goto-char next))))
      (add-text-properties start end props))))

(defun magit-section-maybe-add-heading-map (section)
  (when (magit-section-content-p section)
    (let ((start (oref section start))
          (map (oref section keymap)))
      (when (symbolp map)
        (setq map (symbol-value map)))
      (put-text-property
       start
       (magit--eol-position start)
       'keymap (if map
                   (make-composed-keymap
                    (list map magit-section-heading-map))
                 magit-section-heading-map)))))

(defun magit-section-maybe-remove-heading-map (section)
  (with-slots (start content end keymap) section
    (when (= content end)
      (put-text-property start end 'keymap
                         (if (symbolp keymap) (symbol-value keymap) keymap)))))

(defun magit-insert-child-count (section)
  "Modify SECTION's heading to contain number of child sections.

If `magit-section-show-child-count' is non-nil and the SECTION
has children and its heading ends with \":\", then replace that
with \" (N)\", where N is the number of child sections.

This function is called by `magit-insert-section' after that has
evaluated its BODY.  Admittedly that's a bit of a hack."
  (let (content count)
    (cond
     ((not (and magit-section-show-child-count
                (setq content (oref section content))
                (setq count (length (oref section children)))
                (> count 0))))
     ((eq (char-before (- content 1)) ?:)
      (save-excursion
        (goto-char (- content 2))
        (insert (magit--propertize-face (format " (%s)" count)
                                        'magit-section-child-count))
        (delete-char 1)))
     ((and (eq (char-before (- content 4)) ?\s)
           (eq (char-before (- content 3)) ?\()
           (eq (char-before (- content 2)) ?t )
           (eq (char-before (- content 1)) ?\)))
      (save-excursion
        (goto-char (- content 3))
        (delete-char 1)
        (insert (format "%s" count)))))))

(defun magit-section--opportunistic-wash (section)
  (when-let ((washer (oref section washer)))
    (oset section washer nil)
    (let ((inhibit-read-only t)
          (magit-insert-section--parent section)
          (magit-insert-section--current section))
      (save-excursion
        (goto-char (oref section end))
        (oset section content (point-marker))
        (funcall washer)
        (oset section end (point-marker))))
    (setq magit-section-highlight-force-update t)))

;;; Highlight

(defvar magit-section--refreshed-buffers nil)

(defun magit-section-pre-command-hook ()
  (when (and (or magit--context-menu-buffer
                 magit--context-menu-section)
             (not (eq (ignore-errors
                        (event-basic-type (aref (this-command-keys) 0)))
                      'mouse-3)))
    ;; This is the earliest opportunity to clean up after an aborted
    ;; context-menu because that neither causes the command that created
    ;; the menu to abort nor some abortion hook to be run.  It is not
    ;; possible to update highlighting before the first command invoked
    ;; after the menu is aborted.  Here we can only make sure it is
    ;; updated afterwards.
    (magit-menu-highlight-point-section))
  (setq magit-section--refreshed-buffers nil)
  (setq magit-section-pre-command-region-p (region-active-p))
  (setq magit-section-pre-command-section (magit-current-section))
  (setq magit-section-focused-sections nil))

(defun magit-section-post-command-hook ()
  (let ((window (selected-window)))
    ;; The command may have used `set-window-buffer' to change
    ;; the window's buffer without changing the current buffer.
    (when (eq (current-buffer) (window-buffer window))
      (cursor-sensor-move-to-tangible window)
      (when (or magit--context-menu-buffer
                magit--context-menu-section)
        (magit-menu-highlight-point-section))))
  (unless (memq (current-buffer) magit-section--refreshed-buffers)
    (magit-section-update-highlight))
  (setq magit-section--refreshed-buffers nil))

(defun magit-section-deactivate-mark ()
  (setq magit-section-highlight-force-update t))

(defun magit-section-update-highlight (&optional force)
  (let ((section (magit-current-section))
        (focused (magit-focused-sections)))
    (cond
     ((or force
          magit-section-highlight-force-update
          (xor magit-section-pre-command-region-p (region-active-p))
          (not (eq magit-section-pre-command-section section)))
      (let ((inhibit-read-only t)
            (deactivate-mark nil)
            (selection (magit-region-sections)))
        (mapc #'delete-overlay magit-section-highlight-overlays)
        (mapc #'delete-overlay magit-section-selection-overlays)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-selection-overlays nil)
        (cond ((magit-section--maybe-enable-long-lines-shortcuts))
              ((eq section magit-root-section))
              ((not magit-section-highlight-current)
               (when selection
                 (magit-section-highlight-selection selection)))
              ((not selection)
               (magit-section-highlight section))
              (t
               (mapc #'magit-section-highlight selection)
               (magit-section-highlight-selection selection)))
        (dolist (section (cl-union magit-section-highlighted-sections focused))
          (when (slot-boundp section 'painted)
            (magit-section-update-paint section focused)))
        (restore-buffer-modified-p nil)))
     ((and (eq magit-section-pre-command-section section)
           magit-section-selection-overlays
           (region-active-p)
           (not (magit-region-sections)))
      (mapc #'delete-overlay magit-section-selection-overlays)
      (setq magit-section-selection-overlays nil)))
    (setq magit-section-highlight-force-update nil)
    (magit-section-maybe-paint-visibility-ellipses)))

(cl-defmethod magit-section-highlight ((section magit-section))
  (pcase-let*
      (((eieio start content end children heading-highlight-face) section)
       (headlight heading-highlight-face)
       (selective (magit-section-selective-highlight-p section)))
    (cond
     (selective
      (magit-section-highlight-range start (or content end) headlight)
      (cond (children
             (let ((child-start (oref (car children) start)))
               (when (and content (< content child-start))
                 (magit-section-highlight-range content child-start)))
             (mapc #'magit-section-highlight children))
            ((and content (not (slot-boundp section 'painted)))
             (magit-section-highlight-range content end))
            ;; Unfortunate kludge for delayed hunk refinement.
            ((magit-section--refine section))))
     (headlight
      (magit-section-highlight-range start (or content end) headlight)
      (when content
        (magit-section-highlight-range (if headlight content start) end)))
     ((magit-section-highlight-range start end)))))

(defun magit-section-highlight-selection (selection)
  (when magit-section-highlight-selection
    (dolist (sibling selection)
      (with-slots (start content end heading-selection-face) sibling
        (let ((ov (make-overlay start (or content end) nil t)))
          (overlay-put ov 'font-lock-face
                       (or heading-selection-face
                           'magit-section-heading-selection))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'priority '(nil . 9))
          (push ov magit-section-selection-overlays)
          ov)))))

(defun magit-section-highlight-range (start end &optional face)
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'font-lock-face (or face 'magit-section-highlight))
    (overlay-put ov 'evaporate t)
    (push ov magit-section-highlight-overlays)
    ov))

(defun magit-section-selective-highlight-p (section &optional as-child)
  (or (oref section selective-highlight)
      (and as-child
           (oref section heading-highlight-face))
      (slot-boundp section 'painted)
      (and-let* ((children (oref section children)))
        (magit-section-selective-highlight-p (car children) t))))

;;; Paint

(defun magit-section-update-paint (section focused-sections)
  (cl-flet ((paint (highlight)
              (let ((inhibit-read-only t))
                (save-excursion
                  (goto-char (oref section start))
                  (magit-section-paint section highlight))))
            (unregister ()
              (setq magit-section-highlighted-sections
                    (delq section magit-section-highlighted-sections))))
    (if (magit-section-hidden section)
        ;; If the section is highlighted but unfocused, it remains
        ;; highlighted, but `magit-section--opportunistic-paint' via
        ;; `magit-section-show' will unhighlight on expansion, and
        ;; before then (or if a refresh occurs first) it doesn't matter.
        (unregister)
      (pcase (list (if (memq section focused-sections) 'focus 'unfocus)
                   (oref section painted))
        (`(focus ,(or 'nil 'plain))
         (paint t)
         (cl-pushnew section magit-section-highlighted-sections))
        (`(focus highlight)
         (cl-pushnew section magit-section-highlighted-sections))
        (`(unfocus ,(or 'nil 'highlight))
         (paint nil)
         (unregister))
        ('(unfocus plain)
         (unregister))))))

(cl-defmethod magit-section-paint ((section magit-section) _highlight)
  (error "Slot `paint' bound but `magit-section-paint' not implemented for `%s'"
         (eieio-object-class-name section)))

(defun magit-section--opportunistic-paint (section)
  (when (and (not (oref section hidden))
             (slot-boundp section 'painted))
    (if magit--refreshing-buffer-p
        ;; Defer to `magit-section-update-highlight'.
        (unless (oref section painted)
          (cl-pushnew section magit-section-highlighted-sections))
      (magit-section-update-paint section (magit-focused-sections)))))

(cl-defmethod magit-section--refine ((_section magit-section)))

;;; Long Lines

(defvar magit-show-long-lines-warning t)

(defun magit-section--maybe-enable-long-lines-shortcuts ()
  (and (fboundp 'long-line-optimizations-p)
       (long-line-optimizations-p)
       (prog1 t
         (message "Enabling long lines shortcuts in %S" (current-buffer))
         (kill-local-variable 'redisplay-highlight-region-function)
         (kill-local-variable 'redisplay-unhighlight-region-function)
         (when magit-show-long-lines-warning
           (setq magit-show-long-lines-warning nil)
           (display-warning 'magit (format "\
Emacs has enabled redisplay shortcuts
in this buffer because there are lines whose length go beyond
`long-line-threshold' (%s characters).  As a result, section
highlighting and the special appearance of the region has been
disabled.

These shortcuts remain enabled, even once there no longer are
any long lines in this buffer.  To disable them again, kill
and recreate the buffer.

This message won't be shown for this session again.  To disable
it for all future sessions, set `magit-show-long-lines-warning'
to nil." (bound-and-true-p long-line-threshold)) :warning)))))

;;; Successor

(cl-defgeneric magit-section-get-relative-position (section))

(cl-defmethod magit-section-get-relative-position ((section magit-section))
  (let ((start (oref section start))
        (point (magit-point)))
    (list (- (line-number-at-pos point)
             (line-number-at-pos start))
          (- point (line-beginning-position)))))

(cl-defgeneric magit-section-goto-successor ())

(cl-defmethod magit-section-goto-successor ((section magit-section)
                                            line char &optional _arg)
  (or (magit-section-goto-successor--same section line char)
      (magit-section-goto-successor--related section)))

(defun magit-section-goto-successor--same (section line char)
  (let ((ident (magit-section-ident section)))
    (and-let* ((found (magit-get-section ident)))
      (let ((start (oref found start)))
        (goto-char start)
        (unless (eq found magit-root-section)
          (ignore-errors
            (forward-line line)
            (forward-char char))
          (unless (eq (magit-current-section) found)
            (goto-char start)))
        t))))

(defun magit-section-goto-successor--related (section)
  (and-let* ((found (magit-section-goto-successor--related-1 section)))
    (goto-char (if (eq (oref found type) 'button)
                   (point-min)
                 (oref found start)))))

(defun magit-section-goto-successor--related-1 (section)
  (or (and-let* ((alt (pcase (oref section type)
                        ('staged 'unstaged)
                        ('unstaged 'staged)
                        ('unpushed 'unpulled)
                        ('unpulled 'unpushed))))
        (magit-get-section `((,alt) (status))))
      (and-let* ((next (car (magit-section-siblings section 'next))))
        (magit-get-section (magit-section-ident next)))
      (and-let* ((prev (car (magit-section-siblings section 'prev))))
        (magit-get-section (magit-section-ident prev)))
      (and-let* ((parent (oref section parent)))
        (or (magit-get-section (magit-section-ident parent))
            (magit-section-goto-successor--related-1 parent)))))

;;; Region

(defvar-local magit-section--region-overlays nil)

(defun magit-section--delete-region-overlays ()
  (mapc #'delete-overlay magit-section--region-overlays)
  (setq magit-section--region-overlays nil))

(defun magit-section--highlight-region (start end window rol)
  (magit-section--delete-region-overlays)
  (if (and magit-section-highlight-selection
           (not magit-section-keep-region-overlay)
           (or (magit-region-sections)
               (run-hook-with-args-until-success 'magit-region-highlight-hook
                                                 (magit-current-section)))
           (not (= (line-number-at-pos start)
                   (line-number-at-pos end)))
           ;; (not (eq (car-safe last-command-event) 'mouse-movement))
           )
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun magit-section--unhighlight-region (rol)
  (magit-section--delete-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

;;; Visibility

(defvar-local magit-section-visibility-cache nil)
(put 'magit-section-visibility-cache 'permanent-local t)

(defun magit-section-cached-visibility (section)
  "Return the visibility cached for SECTION.
When `magit-section-preserve-visibility' is nil, return nil."
  (and magit-section-preserve-visibility
       (cdr (assoc (magit-section-ident section)
                   magit-section-visibility-cache))))

(cl-defun magit-section-cache-visibility
    (&optional (section magit-insert-section--current))
  "Cache SECTION's current visibility."
  (setf (compat-call alist-get
                     (magit-section-ident section)
                     magit-section-visibility-cache
                     nil nil #'equal)
        (if (oref section hidden) 'hide 'show)))

(cl-defun magit-section-maybe-cache-visibility
    (&optional (section magit-insert-section--current))
  (when (or (eq magit-section-cache-visibility t)
            (memq (oref section type)
                  magit-section-cache-visibility))
    (magit-section-cache-visibility section)))

(defun magit-section-maybe-update-visibility-indicator (section)
  (when (and magit-section-visibility-indicator
             (magit-section-content-p section))
    (let* ((beg (oref section start))
           (eoh (magit--eol-position beg)))
      (cond
       ((symbolp (car-safe magit-section-visibility-indicator))
        (let ((ov (magit--overlay-at beg 'magit-vis-indicator 'fringe)))
          (unless ov
            (setq ov (make-overlay beg eoh nil t))
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'magit-vis-indicator 'fringe))
          (overlay-put
           ov 'before-string
           (propertize "fringe" 'display
                       (list 'left-fringe
                             (if (oref section hidden)
                                 (car magit-section-visibility-indicator)
                               (cdr magit-section-visibility-indicator))
                             'fringe)))))
       ((stringp (car-safe magit-section-visibility-indicator))
        (let ((ov (magit--overlay-at (1- eoh) 'magit-vis-indicator 'eoh)))
          (cond ((oref section hidden)
                 (unless ov
                   (setq ov (make-overlay (1- eoh) eoh))
                   (overlay-put ov 'evaporate t)
                   (overlay-put ov 'magit-vis-indicator 'eoh))
                 (overlay-put ov 'after-string
                              (car magit-section-visibility-indicator)))
                (ov
                 (delete-overlay ov)))))))))

(defvar-local magit--ellipses-sections nil)

(defun magit-section-maybe-paint-visibility-ellipses ()
  ;; This is needed because we hide the body instead of "the body
  ;; except the final newline and additionally the newline before
  ;; the body"; otherwise we could use `buffer-invisibility-spec'.
  (when (stringp (car-safe magit-section-visibility-indicator))
    (let* ((sections (append magit--ellipses-sections
                             (setq magit--ellipses-sections
                                   (or (magit-region-sections)
                                       (list (magit-current-section))))))
           (beg (mapcar (##oref % start) sections))
           (end (mapcar (##oref % end)   sections)))
      (when (region-active-p)
        ;; This ensures that the region face is removed from ellipses
        ;; when the region becomes inactive, but fails to ensure that
        ;; all ellipses within the active region use the region face,
        ;; because the respective overlay has not yet been updated at
        ;; this time.  The magit-selection face is always applied.
        (push (region-beginning) beg)
        (push (region-end)       end))
      (setq beg (apply #'min beg))
      (setq end (apply #'max end))
      (dolist (ov (overlays-in beg end))
        (when (eq (overlay-get ov 'magit-vis-indicator) 'eoh)
          (overlay-put
           ov 'after-string
           (propertize
            (car magit-section-visibility-indicator) 'font-lock-face
            (let ((pos (overlay-start ov)))
              (delq nil (nconc (mapcar (##overlay-get % 'font-lock-face)
                                       (overlays-at pos))
                               (list (get-char-property
                                      pos 'font-lock-face))))))))))))

(defun magit-section-maybe-remove-visibility-indicator (section)
  (when (and magit-section-visibility-indicator
             (= (oref section content)
                (oref section end)))
    (dolist (o (overlays-in (oref section start)
                            (1+ (magit--eol-position (oref section start)))))
      (when (overlay-get o 'magit-vis-indicator)
        (delete-overlay o)))))

(defvar-local magit-section--opened-sections nil)

(defun magit-section--open-temporarily (beg end)
  (save-excursion
    (goto-char beg)
    (let ((section (magit-current-section)))
      (while section
        (let ((content (oref section content)))
          (if (and (magit-section-invisible-p section)
                   (<= (or content (oref section start))
                       beg
                       (oref section end)))
              (progn
                (when content
                  (magit-section-show section)
                  (push section magit-section--opened-sections))
                (setq section (oref section parent)))
            (setq section nil))))))
  (or (eq search-invisible t)
      (not (isearch-range-invisible beg end))))

(define-advice isearch-clean-overlays (:around (fn) magit-mode)
  (if (derived-mode-p 'magit-mode)
      (let ((pos (point)))
        (dolist (section magit-section--opened-sections)
          (unless (<= (oref section content) pos (oref section end))
            (magit-section-hide section)))
        (setq magit-section--opened-sections nil))
    (funcall fn)))

(defun magit-section-reveal (section)
  (while section
    (when (oref section hidden)
      (magit-section-show section))
    (setq section (oref section parent))))

;;; Utilities

(cl-defun magit-section-selected-p (section &optional (selection nil sselection))
  (and (not (eq section magit-root-section))
       (or  (eq section (magit-current-section))
            (memq section (if sselection
                              selection
                            (setq selection (magit-region-sections))))
            (and-let* ((parent (oref section parent)))
              (magit-section-selected-p parent selection)))))

(defun magit-section-parent-value (section)
  (and-let* ((parent (oref section parent)))
    (oref parent value)))

(defun magit-section-siblings (section &optional direction)
  "Return a list of the sibling sections of SECTION.

If optional DIRECTION is `prev', then return siblings that come
before SECTION.  If it is `next', then return siblings that come
after SECTION.  For all other values, return all siblings
excluding SECTION itself."
  (and-let* ((parent (oref section parent))
             (siblings (oref parent children)))
    (pcase direction
      ('prev  (cdr (member section (reverse siblings))))
      ('next  (cdr (member section siblings)))
      (_      (remq section siblings)))))

(defun magit-focused-sections ()
  "Return a list of the selected sections and all their descendants.
If no sections are selected return a list of the current section and
its descendants, except if that is the root section, in which case
return nil."
  (or magit-section-focused-sections
      (setq magit-section-focused-sections
            (let ((current (magit-current-section)))
              (and (not (eq current magit-root-section))
                   (let (sections)
                     (letrec ((collect (lambda (section)
                                         (mapc collect (oref section children))
                                         (push section sections))))
                       (mapc collect
                             (or (magit-region-sections) (list current))))
                     sections))))))

(defun magit-region-values (&optional condition multiple)
  "Return a list of the values of the selected sections.

Return the values that themselves would be returned by
`magit-region-sections' (which see)."
  (mapcar (##oref % value)
          (magit-region-sections condition multiple)))

(defun magit-region-sections (&optional condition multiple)
  "Return a list of the selected sections.

When the region is active and constitutes a valid section
selection, then return a list of all selected sections.  This is
the case when the region begins in the heading of a section and
ends in the heading of the same section or in that of a sibling
section.  If optional MULTIPLE is non-nil, then the region cannot
begin and end in the same section.

When the selection is not valid, then return nil.  In this case,
most commands that can act on the selected sections will instead
act on the section at point.

When the region looks like it would in any other buffer then
the selection is invalid.  When the selection is valid then the
region uses the `magit-section-highlight' face.  This does not
apply to diffs where things get a bit more complicated, but even
here if the region looks like it usually does, then that's not
a valid selection as far as this function is concerned.

If optional CONDITION is non-nil, then the selection not only
has to be valid; all selected sections additionally have to match
CONDITION, or nil is returned.  See `magit-section-match' for the
forms CONDITION can take."
  (and (region-active-p)
       (let* ((rbeg (region-beginning))
              (rend (region-end))
              (sbeg (magit-section-at rbeg))
              (send (magit-section-at rend)))
         ;; It should be possible to select a single section using
         ;; `set-mark-command', so don't use `use-region-p' above.
         ;; We still have to prevent the selection overlay from
         ;; being flashed when clicking inside a section, which
         ;; the first condition accomplishes:
         (and (or (not (eq this-command #'mouse-drag-region))
                  (> rend rbeg))
              send
              (not (eq send magit-root-section))
              (not (and (eq send sbeg)
                        (or multiple
                            (> rend rbeg))))
              (let ((siblings (cons sbeg (magit-section-siblings sbeg 'next)))
                    (sections ()))
                (and (memq send siblings)
                     (magit-section-position-in-heading-p sbeg rbeg)
                     (magit-section-position-in-heading-p send rend)
                     (progn
                       (while siblings
                         (push (car siblings) sections)
                         (when (eq (pop siblings) send)
                           (setq siblings nil)))
                       (setq sections (nreverse sections))
                       (and (or (not condition)
                                (seq-every-p (##magit-section-match condition %)
                                             sections))
                            sections))))))))

(defun magit-map-sections (function &optional section)
  "Apply FUNCTION to all sections for side effects only, depth first.
If optional SECTION is non-nil, only map over that section and
its descendants, otherwise map over all sections in the current
buffer, ending with `magit-root-section'."
  (let ((section (or section magit-root-section)))
    (mapc (##magit-map-sections function %)
          (oref section children))
    (funcall function section)))

(defun magit-section-position-in-heading-p (&optional section pos)
  "Return t if POSITION is inside the heading of SECTION.
POSITION defaults to point and SECTION defaults to the
current section."
  (unless section
    (setq section (magit-current-section)))
  (unless pos
    (setq pos (point)))
  (ignore-errors ; Allow navigating broken sections.
    (and section
         (>= pos (oref section start))
         (<  pos (or (oref section content)
                     (oref section end)))
         t)))

(defun magit-section-internal-region-p (&optional section)
  "Return t if the region is active and inside SECTION's body.
If optional SECTION is nil, use the current section."
  (and (region-active-p)
       (or section (setq section (magit-current-section)))
       (let ((beg (magit-section-at (region-beginning))))
         (and (eq beg (magit-section-at (region-end)))
              (eq beg section)))
       (not (or (magit-section-position-in-heading-p section (region-beginning))
                (magit-section-position-in-heading-p section (region-end))))
       t))

(defun magit-wash-sequence (function)
  "Repeatedly call FUNCTION until it returns nil or eob is reached.
FUNCTION has to move point forward or return nil."
  (while (and (not (eobp)) (funcall function))))

;;;###autoload
(defun magit-add-section-hook (hook function &optional at append local)
  "Add to the value of section hook HOOK the function FUNCTION.

Add FUNCTION at the beginning of the hook list unless optional
APPEND is non-nil, in which case FUNCTION is added at the end.
If FUNCTION already is a member, then move it to the new location.

If optional AT is non-nil and a member of the hook list, then
add FUNCTION next to that instead.  Add before or after AT, or
replace AT with FUNCTION depending on APPEND.  If APPEND is the
symbol `replace', then replace AT with FUNCTION.  For any other
non-nil value place FUNCTION right after AT.  If nil, then place
FUNCTION right before AT.  If FUNCTION already is a member of the
list but AT is not, then leave FUNCTION where ever it already is.

If optional LOCAL is non-nil, then modify the hook's buffer-local
value rather than its global value.  This makes the hook local by
copying the default value.  That copy is then modified.

HOOK should be a symbol.  If HOOK is void, it is first set to nil.
HOOK's value must not be a single hook function.  FUNCTION should
be a function that takes no arguments and inserts one or multiple
sections at point, moving point forward.  FUNCTION may choose not
to insert its section(s), when doing so would not make sense.  It
should not be abused for other side-effects.  To remove FUNCTION
again use `remove-hook'."
  (unless (boundp hook)
    (error "Cannot add function to undefined hook variable %s" hook))
  (unless (default-boundp hook)
    (set-default hook nil))
  (let ((value (if local
                   (if (local-variable-p hook)
                       (symbol-value hook)
                     (unless (local-variable-if-set-p hook)
                       (make-local-variable hook))
                     (copy-sequence (default-value hook)))
                 (default-value hook))))
    (if at
        (when (setq at (member at value))
          (setq value (delq function value))
          (cond ((eq append 'replace)
                 (setcar at function))
                (append
                 (push function (cdr at)))
                (t
                 (push (car at) (cdr at))
                 (setcar at function))))
      (setq value (delq function value)))
    (unless (member function value)
      (setq value (if append
                      (append value (list function))
                    (cons function value))))
    (when (eq append 'replace)
      (setq value (delq at value)))
    (if local
        (set hook value)
      (set-default hook value))))

(defvar-local magit-disabled-section-inserters nil)

(defun magit-disable-section-inserter (fn)
  "Disable the section inserter FN in the current repository.
It is only intended for use in \".dir-locals.el\" and
\".dir-locals-2.el\".  Also see info node `(magit)Per-Repository
Configuration'."
  (cl-pushnew fn magit-disabled-section-inserters))

(put 'magit-disable-section-inserter 'safe-local-eval-function t)

(defun magit-run-section-hook (hook &rest args)
  "Run HOOK with ARGS, warning about invalid entries."
  (let ((entries (symbol-value hook)))
    (unless (listp entries)
      (setq entries (list entries)))
    (when-let ((invalid (seq-remove #'functionp entries)))
      (message "`%s' contains entries that are no longer valid.
%s\nUsing standard value instead.  Please re-configure hook variable."
               hook
               (mapconcat (##format "  `%s'" %) invalid "\n"))
      (sit-for 5)
      (setq entries (eval (car (get hook 'standard-value)))))
    (dolist (entry entries)
      (let ((magit--current-section-hook (cons (list hook entry)
                                               magit--current-section-hook)))
        (unless (memq entry magit-disabled-section-inserters)
          (if (bound-and-true-p magit-refresh-verbose)
              (let ((time (benchmark-elapse (apply entry args))))
                (message "  %-50s %f %s" entry time
                         (cond ((> time 0.03) "!!")
                               ((> time 0.01) "!")
                               (t ""))))
            (apply entry args)))))))

(cl-defun magit--overlay-at (pos prop &optional (val nil sval) testfn)
  (cl-find-if (lambda (o)
                (let ((p (overlay-properties o)))
                  (and (plist-member p prop)
                       (or (not sval)
                           (funcall (or testfn #'eql)
                                    (plist-get p prop)
                                    val)))))
              (overlays-at pos t)))

(defun magit-face-property-all (face string)
  "Return non-nil if FACE is present in all of STRING."
  (catch 'missing
    (let ((pos 0))
      (while (setq pos (next-single-property-change pos 'font-lock-face string))
        (let ((val (get-text-property pos 'font-lock-face string)))
          (unless (if (consp val)
                      (memq face val)
                    (eq face val))
            (throw 'missing nil))))
      (not pos))))

(defun magit--add-face-text-property ( beg end face
                                       &optional append object adopt-face)
  "Like `add-face-text-property' but for `font-lock-face'.
If optional ADOPT-FACE, then replace `face' with `font-lock-face'
first.  The latter is a hack, which is likely to be removed again."
  (when (stringp object)
    (unless beg (setq beg 0))
    (unless end (setq end (length object))))
  (when adopt-face
    (let ((beg beg)
          (end end))
      (while (< beg end)
        (let ((pos (next-single-property-change beg 'face object end))
              (val (get-text-property beg 'face object)))
          ;; We simply assume font-lock-face is not also set.
          (put-text-property beg pos 'font-lock-face val object)
          (remove-list-of-text-properties beg pos '(face) object)
          (setq beg pos)))))
  (while (< beg end)
    (let* ((pos (next-single-property-change beg 'font-lock-face object end))
           (val (get-text-property beg 'font-lock-face object))
           (val (ensure-list val)))
      (put-text-property beg pos 'font-lock-face
                         (if append
                             (append val (list face))
                           (cons face val))
                         object)
      (setq beg pos)))
  object)

(defun magit--propertize-face (string face)
  (propertize string 'face face 'font-lock-face face))

(defun magit--put-face (beg end face string)
  (put-text-property beg end 'face face string)
  (put-text-property beg end 'font-lock-face face string))

(defun magit--bolp (pos)
  "Return t if POS is at the beginning of a line.
This is like moving to POS and then calling `bolp'."
  (save-excursion (goto-char pos) (bolp)))

(defun magit--eolp (pos)
  "Return t if POS is at the end of a line.
This is like moving to POS and then calling `eolp'."
  (save-excursion (goto-char pos) (bolp)))

(defun magit--bol-position (pos)
  "Return the position at the beginning of the line containing POS.
This is like moving to POS and then calling `pos-bol'."
  (save-excursion (goto-char pos) (pos-bol)))

(defun magit--eol-position (pos)
  "Return the position at the end of the line containing POS.
This is like moving to POS and then calling `pos-eol'."
  (save-excursion (goto-char pos) (pos-eol)))

;;; Imenu Support

(defvar-local magit--imenu-group-types nil)
(defvar-local magit--imenu-item-types nil)

(defun magit--imenu-create-index ()
  ;; If `which-function-mode' is active, then the create-index
  ;; function is called at the time the major-mode is being enabled.
  ;; Modes that derive from `magit-mode' have not populated the buffer
  ;; at that time yet, so we have to abort.
  (and magit-root-section
       (or magit--imenu-group-types
           magit--imenu-item-types)
       (let ((index
              (mapcan
               (lambda (section)
                 (cond
                  (magit--imenu-group-types
                   (and (if (eq (car-safe magit--imenu-group-types) 'not)
                            (not (magit-section-match
                                  (cdr magit--imenu-group-types)
                                  section))
                          (magit-section-match magit--imenu-group-types section))
                        (and-let* ((children (oref section children)))
                          `((,(magit--imenu-index-name section)
                             ,@(mapcar (##cons (magit--imenu-index-name %)
                                               (oref % start))
                                       children))))))
                  (magit--imenu-item-types
                   (and (magit-section-match magit--imenu-item-types section)
                        `((,(magit--imenu-index-name section)
                           . ,(oref section start)))))))
               (oref magit-root-section children))))
         (if (and magit--imenu-group-types (symbolp magit--imenu-group-types))
             (cdar index)
           index))))

(defun magit--imenu-index-name (section)
  (let ((heading (buffer-substring-no-properties
                  (oref section start)
                  (1- (or (oref section content)
                          (oref section end))))))
    (save-match-data
      (cond
       ((and (magit-section-match [commit logbuf] section)
             (string-match "[^ ]+\\([ *|]*\\).+" heading))
        (replace-match " " t t heading 1))
       ((magit-section-match
         '([branch local branchbuf] [tag tags branchbuf]) section)
        (oref section value))
       ((magit-section-match [branch remote branchbuf] section)
        (concat (oref (oref section parent) value) "/"
                (oref section value)))
       ((string-match " ([0-9]+)\\'" heading)
        (substring heading 0 (match-beginning 0)))
       (t heading)))))

(defun magit--imenu-goto-function (_name position &rest _rest)
  "Go to the section at POSITION.
Make sure it is visible, by showing its ancestors where
necessary.  For use as `imenu-default-goto-function' in
`magit-mode' buffers."
  (goto-char position)
  (let ((section (magit-current-section)))
    (while (setq section (oref section parent))
      (when (oref section hidden)
        (magit-section-show section)))))

;;; Bookmark support

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-make-record-default "bookmark"
                  (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark-name-or-record prop))
(declare-function bookmark-prop-set "bookmark" (bookmark-name-or-record prop val))

(cl-defgeneric magit-bookmark-get-filename ()
  (or (buffer-file-name) (buffer-name)))

(cl-defgeneric magit-bookmark-get-value (bookmark mode))

(cl-defgeneric magit-bookmark--get-child-value (section)
  (oref section value))

(cl-defgeneric magit-bookmark-get-buffer-create (bookmark mode))

(defun magit--make-bookmark ()
  "Create a bookmark for the current Magit buffer.
Input values are the major-mode's `magit-bookmark-name' method,
and the buffer-local values of the variables referenced in its
`magit-bookmark-variables' property."
  (require 'bookmark)
  (if (plist-member (symbol-plist major-mode) 'magit-bookmark-variables)
      ;; `bookmark-make-record-default's return value does not match
      ;; (NAME . ALIST), even though it is used as the default value
      ;; of `bookmark-make-record-function', which states that such
      ;; functions must do that.  See #4356.
      (let ((bookmark (cons nil (bookmark-make-record-default 'no-file))))
        (bookmark-prop-set bookmark 'handler  #'magit--handle-bookmark)
        (bookmark-prop-set bookmark 'mode     major-mode)
        (bookmark-prop-set bookmark 'filename (magit-bookmark-get-filename))
        (bookmark-prop-set bookmark 'defaults (list (magit-bookmark-name)))
        (magit-bookmark-get-value bookmark)
        (bookmark-prop-set
         bookmark 'magit-hidden-sections
         (seq-keep (##and (oref % hidden)
                          (cons (oref % type)
                                (magit-bookmark--get-child-value %)))
                   (oref magit-root-section children)))
        bookmark)
    (user-error "Bookmarking is not implemented for %s buffers" major-mode)))

;;;###autoload
(defun magit--handle-bookmark (bookmark)
  "Open a bookmark created by `magit--make-bookmark'.

Call the generic function `magit-bookmark-get-buffer-create' to get
the appropriate buffer without displaying it.

Then call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'."
  (require (quote magit-bookmark) nil t)
  (let ((buffer (magit-bookmark-get-buffer-create
                 bookmark
                 (bookmark-prop-get bookmark 'mode))))
    (set-buffer buffer) ; That is the interface we have to adhere to.
    (when-let ((hidden (bookmark-prop-get bookmark 'magit-hidden-sections)))
      (with-current-buffer buffer
        (dolist (child (oref magit-root-section children))
          (if (member (cons (oref child type)
                            (oref child value))
                      hidden)
              (magit-section-hide child)
            (magit-section-show child)))))
    ;; Compatibility with `bookmark+' package.  See #4356.
    (when (bound-and-true-p bmkp-jump-display-function)
      (funcall bmkp-jump-display-function (current-buffer)))
    nil))

(put 'magit--handle-bookmark 'bookmark-handler-type "Magit")

(cl-defgeneric magit-bookmark-name ()
  "Return name for bookmark to current buffer."
  (format "%s%s"
          (substring (symbol-name major-mode) 0 -5)
          (if-let ((vars (get major-mode 'magit-bookmark-variables)))
              (mapcan (##ensure-list (symbol-value %)) vars)
            "")))

;;; Bitmaps

(define-fringe-bitmap 'magit-fringe-bitmap+
  [#b00000000
   #b00011000
   #b00011000
   #b01111110
   #b01111110
   #b00011000
   #b00011000
   #b00000000])

(define-fringe-bitmap 'magit-fringe-bitmap-
  [#b00000000
   #b00000000
   #b00000000
   #b01111110
   #b01111110
   #b00000000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'magit-fringe-bitmap>
  [#b01100000
   #b00110000
   #b00011000
   #b00001100
   #b00011000
   #b00110000
   #b01100000
   #b00000000])

(define-fringe-bitmap 'magit-fringe-bitmapv
  [#b00000000
   #b10000010
   #b11000110
   #b01101100
   #b00111000
   #b00010000
   #b00000000
   #b00000000])

(define-fringe-bitmap 'magit-fringe-bitmap-bold>
  [#b11100000
   #b01110000
   #b00111000
   #b00011100
   #b00011100
   #b00111000
   #b01110000
   #b11100000])

(define-fringe-bitmap 'magit-fringe-bitmap-boldv
  [#b10000001
   #b11000011
   #b11100111
   #b01111110
   #b00111100
   #b00011000
   #b00000000
   #b00000000])

;;; _
(provide 'magit-section)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("match-string" . "match-string")
;;   ("match-str" . "match-string-no-properties"))
;; End:
;;; magit-section.el ends here
