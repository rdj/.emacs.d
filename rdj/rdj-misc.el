;; Don't require full "yes" or "no" answers.
(fset 'yes-or-no-p 'y-or-n-p)


;; Use pathname to get unique buffer names instead of <2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'rdj-misc)
