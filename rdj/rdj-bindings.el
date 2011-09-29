(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)
(global-set-key (kbd "C-c C-z") 'rdj-smartsize-frame)

(provide 'rdj-bindings)
