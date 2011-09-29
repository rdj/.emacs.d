;;; Behavior

;; Don't require full "yes" or "no" answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Nobody double-spaces after periods in the 21st century.
(setq sentence-end-double-space nil)

;; If I turn on truncate-lines, I mean it
(setq truncate-partial-width-windows nil)

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


;;; Modes

;; Default to fundamental
(setq default-major-mode 'fundamental-mode)

;; Font lock
(global-font-lock-mode 't)
(setq font-lock-maximum-decoration 't)

;; Abbrevations
(setq default-abbrev-mode 't)


(provide 'rdj-misc)
