;; rdj-turds.el --- Ryan D Johnson's Emacs configuration

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

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar rdj-autosave-dir (concat user-emacs-directory "autosaves/"))
(make-directory rdj-autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "\\`#.*#\\'" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat rdj-autosave-dir (if buffer-file-name
                               (concat "#" (file-name-nondirectory buffer-file-name) "#")
                               (expand-file-name (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar rdj-backup-dir (concat user-emacs-directory "backups/"))
(setq backup-directory-alist (list (cons "." rdj-backup-dir)))

;; Disable backup files for su/sudo edited files. They are a security risk.
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

(provide 'rdj-turds)
