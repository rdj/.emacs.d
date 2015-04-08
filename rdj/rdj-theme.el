;; rdj-theme.el --- Ryan D Johnson's Emacs configuration

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

;; The variable redisplay-dont-pause, when set to t, will cause Emacs
;; to fully redraw the display before it processes queued input
;; events. This may have slight performance implications if you’re
;; aggressively mouse scrolling a document or rely on your keyboard’s
;; auto repeat feature. For most of us, myself included, it’s probably
;; a no-brainer to switch it on.
(setq redisplay-dont-pause t)

(require 'unicode-fonts)
(unicode-fonts-setup)

;; Replace audible bell with a subtle visual bell (default visible
;; bell is awful)
(defun rdj-modeline-visible-bell ()
 "Briefly inverts the modeline face (for use as ring-bell-function)"
 (invert-face 'mode-line)
 (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'rdj-modeline-visible-bell)

;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Highlight paren expressions
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;; Show file position
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; Window title
(defun rdj-shorten-filename (filename)
  (replace-regexp-in-string (format "^%s" (getenv "HOME")) "~" filename))
(setq frame-title-format (list "%b"
                               (list 'buffer-file-name
                                     (list " : " '(:eval (rdj-shorten-filename buffer-file-name))))
                               (list 'dired-directory
                                     (list " : " '(:eval (rdj-shorten-filename dired-directory))))))


;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn)

(provide 'rdj-theme)
