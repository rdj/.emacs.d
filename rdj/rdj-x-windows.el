;; rdj-x-windows.el --- Ryan D Johnson's Emacs configuration

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

;; Not included on purpose, but didn't want to lose this

;; Cut/paste
(when window-system
;; Paste where I am, not where I click
  (setq-default mouse-yank-at-point 't)
;; Kill/yank should use the WM clipboard ...
  (setq-default x-select-enable-clipboard 't)
;; ... but shouldn't mess with the X 'primary selection' middle click deal
  (setq-default x-select-enable-primary nil))

(defun rdj-disable-x-clipboard ()
  "Turns off the OS clipboard; useful for speeding up slow remote X Windows connections."
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(provide 'rdj-x-windows)
