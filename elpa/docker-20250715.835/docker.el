;;; docker.el --- Interface to Docker  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Package-Version: 20250715.835
;; Package-Revision: 91233a7c559d
;; Package-Requires: ((aio "1.0") (dash "2.19.1") (emacs "26.1") (s "1.13.0") (tablist "1.1") (transient "0.4.3"))

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

;; This package allows you to manipulate docker images, containers & more from Emacs.

;;; Code:
(eval-when-compile
  (setq-local byte-compile-warnings '(not docstrings)))

(require 'docker-compose)
(require 'docker-container)
(require 'docker-image)
(require 'docker-network)
(require 'docker-volume)
(require 'docker-context)

(provide 'docker)

;;; docker.el ends here
