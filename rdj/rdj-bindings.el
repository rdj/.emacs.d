;; rdj-bindings.el --- Ryan D Johnson's Emacs configuration

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

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c TAB") 'rdj-swap-windows)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c C-v") 'revert-buffer)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)
(global-set-key (kbd "C-c C-z") 'rdj-smartsize-frame)

(global-set-key (kbd "C-x f") 'helm-projectile)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-/") 'hippie-expand) ;; overwrite the binding for dabbrev-expand, seems safer than defalias
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-M-|") 'align-regexp)

;; smex is a replacement for M-x that uses ido
(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-≈") 'helm-M-x)

;; Tab should indent region if selected or this line if not
(defun rdj-maybe-tab ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (indent-region (region-beginning) (region-end) nil)
    (c-indent-command)))

(defun tab-indents-region () (local-set-key (kbd "TAB") 'rdj-maybe-tab))
(add-hook 'c-mode-common-hook 'tab-indents-region)

(add-hook 'isearch-mode-hook
          (function
           (lambda ()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))

; cperl-mode rudely overrides my whitespace toggle
(add-hook 'cperl-mode-hook
          (function
           (lambda ()
             (local-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup))))

; org-mode rudely overrides my resize key
(add-hook 'org-mode-hook
          (function
           (lambda ()
             (local-set-key (kbd "C-c C-z") 'rdj-smartsize-frame))))

;; cua-mode is generally terrible, but every once in a while it is
;; handy to use because it lets you use normal region commands on
;; rectangular regions, which are selected with C-RET.
;;
;; So with this binding, I can toggle CUA mode on briefly and then
;; back off.
;;
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-c r") 'cua-mode)

;; Inspired by prelude-move-beginning-of-line but with opposite
;; behavior.
(defun rdj-move-beginning-of-line (arg)
  "Move point back to beginning of line.

Move point to the beginning of the line. If point is already
there, move to the first non-whitespace character on this line.
Effectively toggle between first non-whitespace charcter and the
beginning of the line, choosing beginning of line first.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(global-set-key [remap move-beginning-of-line]
                'rdj-move-beginning-of-line)

(provide 'rdj-bindings)
