(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c TAB") 'rdj-swap-windows)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)
(global-set-key (kbd "C-c C-z") 'rdj-smartsize-frame)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-i") 'rdj-goto-symbol)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-M-|") 'align-regexp)

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

(provide 'rdj-bindings)
