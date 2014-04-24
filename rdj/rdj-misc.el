;;; Behavior

;; By default GC hard kicks in every 0.76 MB allocated
(setq gc-cons-threshold 20000000)

;; Don't require full "yes" or "no" answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Nobody double-spaces after periods in the 21st century.
(setq sentence-end-double-space nil)

;; If I turn on truncate-lines, I mean it
(setq truncate-partial-width-windows nil)

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))
;; Effectively move file name to end of the list
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;; use ido
(setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last")
      ido-use-faces nil ;; disable ido faces to see flx highlights
      )
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1) ;; replacement for ido-enable-flex-matching
(setq ido-use-faces nil)

;; automatically reload files changed in background
(global-auto-revert-mode 1)

;; Use pathname to get unique buffer names instead of <2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;; Whitespace/encoding related

;; UTF-8 FTW
(prefer-coding-system 'utf-8)

;; Always add a final newline
(setq require-final-newline 't)

;; Never use tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Scrolling past the end of the file should not make new lines
(setq next-line-add-newlines nil)


(provide 'rdj-misc)
