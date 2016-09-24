;; init.el --- Ryan D Johnson's Emacs configuration

;; Copyright (C) 1998-2015 Ryan D Johnson

;; Author: Ryan D Johnson <ryandjohnson@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA


;; Added by Package.el Dont't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; (I call package-initailize in rdj-package /rdj)
;(package-initialize)

(require 'cl)
(require 'assoc)

;; My files are in .emacs.d/rdj
(setq rdj-elisp-path (concat user-emacs-directory "rdj/"))
(push rdj-elisp-path load-path)

;; Non-package.el stuff from ~/.emacs.d/vendor
(setq rdj-vendor-elisp-path (concat user-emacs-directory "vendor/"))
(push rdj-vendor-elisp-path load-path)

;; Customize is basically an anti-feature, but some things like theme
;; whitelists according to file hashes are just probably going to live
;; there.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'rdj-utils)

(require 'rdj-early-off)
(require 'rdj-package)
(require 'rdj-fix-path)

(require 'rdj-misc)
(require 'rdj-smartsize)
(require 'rdj-turds)
(require 'rdj-whitespace)

(require 'rdj-prog)

(require 'rdj-bindings)
(require 'rdj-theme)
(require 'rdj-state)
