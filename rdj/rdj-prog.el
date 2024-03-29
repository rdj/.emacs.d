;; rdj-prog.el --- Ryan D Johnson's Emacs configuration

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

(defun rdj-ret-indents () (local-set-key (kbd "RET") 'newline-and-indent))
(defun rdj-no-auto-fill () (auto-fill-mode 0))
(defun rdj-sub-words () (subword-mode 1))

(defun rdj-setup-prog-mode-hook (hook) ;; Some programming modes don't run prog-mode-hook
  "Adds my default programmingish actions to the hook"
  (add-hook hook 'rdj-ret-indents)
  (add-hook hook 'rdj-no-auto-fill)
  (add-hook hook 'rdj-sub-words)
  )

(rdj-setup-prog-mode-hook 'prog-mode-hook)

;; C/C++
(c-add-style "ryan" '("bsd"
  (c-basic-offset . 4)
  (c-offsets-alist
    (arglist-close . 0)
;    (objc-method-intro . 0)
;    (objc-method-args-cont . 0)
;    (objc-method-call-cont . +)
  )
))

(setq-default
  c-echo-syntactic-information-p 't
  c-default-style "ryan"
  c-auto-newline nil
  c-hungry-delete-key nil
)
(rdj-setup-prog-mode-hook 'c-mode-common-hook) ;; vendored cc-mode isn't a prog-mode derived mode

;; CSS
(setq-default css-indent-offset 2)
(rdj-setup-prog-mode-hook 'css-mode-hook) ;; css-mode doesn't run prog-mode-hook

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Perl
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl"     . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5"    . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(setq cperl-autoindent-on-semi t
      cperl-close-paren-offset -4
      cperl-continued-brace-offset -4
      cperl-continued-statement-offset 4
      cperl-extra-newline-before-brace t
      cperl-extra-newline-before-brace-multiline t
      cperl-font-lock t
      cperl-indent-comment-at-column-0 t
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-indent-region-fix-constructs nil
;;      cperl-indent-wrt-brace nil ;; what does this do?
      cperl-invalid-face nil
      cperl-label-offset 0                            ;; FOO:
      cperl-lineup-step 1                           ;; use cperl-indent-level
      cperl-merge-trailing-else nil
      cperl-min-label-indent 0                        ;; FOO:
      cperl-tab-always-indent t                       ;; regardless of line position
)

;; Turning off automatic abbrev-mode -- use C-' to expand
(add-hook 'cperl-mode-hook
          ;; Unfortunately, cperl-mode uses abbrev-mode to do it's electric
          ;; keyword expansion, and there's no way to make it not populate
          ;; those abbrevs (cperl-electric-keywords nil) just keeps cperl-mode
          ;; from turning on abbrev-mode.
          (function (lambda () (abbrev-mode 0))))

;; ruby
(add-hook 'ruby-mode-hook (lambda () (defun ruby-mode-set-encoding () nil)))
(rdj-setup-prog-mode-hook 'ruby-mode-hook)
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.arb\\'" . ruby-mode))

;; ruby-insert-end was removed from ruby-mode.el, see https://groups.google.com/group/emacs-on-rails/msg/565fba8263233c28
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode)))
(setq ruby-electric-expand-delimiters-list '())

;; Text
(add-hook 'text-mode-hook (function (lambda () (flyspell-mode 1))))
(add-hook 'org-mode-hook (function (lambda () (flyspell-mode -1))))

;; XCode Configuration
(add-to-list 'auto-mode-alist '("\\.xcconfig\\'" . conf-mode))

;; XML
(add-to-list 'auto-mode-alist '("\\.xml\\'"    . nxml-mode)) ;; XML Document
(add-to-list 'auto-mode-alist '("\\.xsd\\'"    . nxml-mode)) ;; XML Schema
(add-to-list 'auto-mode-alist '("\\.plist\\'"  . nxml-mode)) ;; Apple plist
(add-to-list 'auto-mode-alist '("\\.wxi\\'"    . nxml-mode)) ;; WiX installer
(add-to-list 'auto-mode-alist '("\\.wxs\\'"    . nxml-mode)) ;; WiX installer
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode)) ;; VS C# project
(add-to-list 'auto-mode-alist '("\\.xaml\\'"   . nxml-mode)) ;; Microsoft XAML

;; HAML
(rdj-setup-prog-mode-hook 'haml-mode-hook)
(add-hook 'haml-mode-hook (function (lambda () (flyspell-prog-mode))))

;; CoffeeScript
(setq-default coffee-tab-width 2)

(projectile-global-mode)
(setq projectile-use-git-grep 't)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; LillyPond music notation
(when rdj-is-mac
  (push "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/" load-path)
  (autoload 'LilyPond-mode "lilypond-mode")
  (add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
  (add-hook 'LilyPond-mode-hook
            (function (lambda ()
                        (define-key LilyPond-mode-map (kbd "C-c C-r") 'recompile)))))


;; plantuml UML diagrams
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-executable-path "/usr/local/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

;; golang
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-split-window-on-execute 't)
(setq py-split-window-on-execute-threshold 2)
(setq py-split-windows-on-execute-function 'split-window-horizontally)
(add-hook 'python-mode-hook 'py-autopep8-mode)
;;(setq python-shell-interpreter "python3")

(provide 'rdj-prog)
