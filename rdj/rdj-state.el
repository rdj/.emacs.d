;; rdj-state.el --- Ryan D Johnson's Emacs configuration

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

;; If there's no desktop state to load, open a couple of files I
;; always have open and split the window.
(require 'desktop)
(setq desktop-path (list user-emacs-directory))
(defun rdj-default-initial-state ()
  "Opens init.el and scratchfile in a split window"
  (interactive)
  (progn
    (find-file "~/scratchfile")
    (split-window-horizontally)
    (find-file "~/.emacs.d/init.el")
    (other-window 1)
    ))
(add-hook 'desktop-no-desktop-file-hook 'rdj-default-initial-state)
(add-hook 'desktop-after-read-hook (function (lambda () (progn
                                                          (split-window-horizontally)
                                                          (previous-buffer)
                                                          (other-window 1)
                                                          ))))

;; clean-buffer-list is a great function that can save you from the
;; insanity of a months-old desktop session with thousands of buffers.
(require 'midnight)
(setq clean-buffer-list-delay-general 7) ;; days
(add-to-list 'clean-buffer-list-kill-never-buffer-names "scratchfile")
(add-to-list 'clean-buffer-list-kill-regexps "^\\*magit.*$")

;; I use window-system as a marker for this being the "main instance"
;; of emacs. I generally use `emacs -nw` from bash for short-lived
;; editing like `sudo emacs -nw /etc/hosts`
(when window-system
  (server-start)
  (setq server-window (selected-frame))
  (desktop-save-mode 1)
  (rdj-smartsize-frame)
  )

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".saveplace"))

(provide 'rdj-state)
