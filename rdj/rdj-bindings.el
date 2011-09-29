(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)

(provide 'rdj-bindings)
