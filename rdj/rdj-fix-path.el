;; rdj-fix-path.el --- Ryan D Johnson's Emacs configuration

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

;; At some point a Lion update broke loading PATH from
;; environment.plist. Thanks to jwiegley for this little elisp gem to
;; load the file manually.
;;
;; Source: https://github.com/jwiegley/dot-emacs/blob/master/init.el

(when rdj-is-mac
  (let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
    (when (file-readable-p plist)
      (let* ((xml (shell-command-to-string
                   (format "/usr/bin/plutil -convert xml1 -o - %s"
                           (shell-quote-argument plist))))
             (root (with-temp-buffer (insert xml)
                                     (xml-parse-region (point-min) (point-max))))
             (dict (cdr (assq 'dict (cdar root)))))

        (while dict
          (if (and (listp (car dict))
                   (eq 'key (caar dict)))
              (setenv (car (cddr (car dict)))
                      (car (cddr (car (cddr dict))))))
          (setq dict (cdr dict))))

      ;; Configure exec-path based on the new PATH
      (setq exec-path nil)
      (mapc (apply-partially #'add-to-list 'exec-path)
            (nreverse (split-string (getenv "PATH") ":"))))))

(provide 'rdj-fix-path)
