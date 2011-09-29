;; Think I need to write a before-save-hook that does
;; whitespace-cleanup for certain major modes, toggleable with a
;; hotkey to avoid fucking up other people's stuff.

(defvar rdj-whitespace-cleanup-major-modes '(
  c++-mode
  c-mode
  cperl-mode
  csharp-mode
  css-mode
  emacs-lisp-mode
  haml-mode
  html-mode
  java-mode
  js2-mode
  latex-mode
  lisp-mode
  perl-mode
  php-mode
  makefile-mode
  nxhtml-mode
  nxml-mode
  objc-mode
  perl-mode
  ruby-mode
  shell-script-mode
  yaml-mode
  )
  "Major modes for which to do whitespace cleanup")

(defun rdj-whitespace-should-cleanup ()
  (memq major-mode rdj-whitespace-cleanup-major-modes)
  )

(defvar rdj-whitespace-should-cleanup-p 'rdj-whitespace-should-cleanup
  "Whether this buffer should have its whitespace cleaned up before save.")
(make-variable-buffer-local 'rdj-whitespace-should-cleanup-p)

(defun rdj-whitespace-cleanup ()
  (let ((flag rdj-whitespace-should-cleanup-p))
    (and flag
	 (symbolp flag)
	 (fboundp flag)
	 (setq flag (funcall flag)))
    (and flag
	 (save-excursion
	   (delete-trailing-whitespace)
	   ))))

(add-hook 'before-save-hook 'rdj-whitespace-cleanup)

;; Allow nuke-trailing-whitespace-p to be set from the modeline
(aput 'safe-local-variable-values 'rdj-whitespace-should-cleanup-p 't)

(provide 'rdj-whitespace)
