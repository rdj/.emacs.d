(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-i") 'toggle-case-fold-search)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)
(global-set-key (kbd "C-c C-z") 'rdj-smartsize-frame)

(add-hook 'isearch-mode-hook
	  (function
	   (lambda ()
	     (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
	     (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
	     (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
	     (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))

(provide 'rdj-bindings)
