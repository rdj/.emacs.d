;; Don't require full "yes" or "no" answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight paren expressions
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Use pathname to get unique buffer names instead of <2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'rdj-misc)
