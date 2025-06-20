;;; gnuplot-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from gnuplot.el

(autoload 'gnuplot-mode "gnuplot" "\
Major mode for editing and executing GNUPLOT scripts.
This was written with version 4.6 of gnuplot in mind, but should
work with newer and older versions.

Report bugs at https://github.com/emacs-gnuplot/gnuplot/issues

                            ------O------

Gnuplot-mode includes two different systems for keyword completion and
documentation lookup: a newer one, `gnuplot-context-sensitive-mode' ,
which is enabled by default, and a older one which extracts keywords
from gnuplot's Info file.  Both systems allow looking up documentation
in the Info file.  With the context-sensitive mode active,
`gnuplot-mode' can also provide Eldoc syntax hints as you type.

                            ------O------

There are several known shortcomings of `gnuplot-mode', version 0.5g
and up.  Many of the shortcomings involve the graphical interface
(refered to as the GUI) to setting arguments to plot options.  Here is
a list:

 1.  Currently there is no way for `gnuplot-mode' to know if information
     sent to gnuplot was correctly plotted.
 2.  \"plot\", \"splot\", and \"fit\" are handled in the GUI, but are
     a bit flaky.  Their arguments may not be read correctly from
     existing text, and continuation lines (common for plot and splot)
     are not supported.
 3.  The GUI does not know how to read from continuation lines.
 4.  Comma separated position arguments to plot options are
     unsupported in the GUI.  Colon separated datafile modifiers (used
     for plot, splot, and fit) are not supported either.  Arguments
     not yet supported by the GUI generate messages printed in grey
     text.
 5.  The GUI handling of \"hidden3d\" is flaky and \"cntrparam\" is
     unsupported.

(fn)" t)
(autoload 'gnuplot-make-buffer "gnuplot" "\
Open a new buffer in `gnuplot-mode'.
When invoked, it switches to a new, empty buffer visiting no file
and then starts `gnuplot-mode'.

It is convenient to bind this function to a global key sequence.  For
example, to make the F10 key open a gnuplot script buffer, put the
following in your .emacs file:
     (global-set-key [(f10)] \\='gnuplot-make-buffer)" t)
(autoload 'run-gnuplot "gnuplot" "\
Run an inferior Gnuplot process." t)
(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
(add-to-list 'interpreter-mode-alist '("gnuplot" . gnuplot-mode))
(register-definition-prefixes "gnuplot" '("gnuplot-"))


;;; Generated autoloads from gnuplot-context.el

(register-definition-prefixes "gnuplot-context" '("gnuplot-context-"))


;;; Generated autoloads from gnuplot-gui.el

(register-definition-prefixes "gnuplot-gui" '("gnuplot-gui-"))

;;; End of scraped data

(provide 'gnuplot-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; gnuplot-autoloads.el ends here
