(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "C-c TAB") 'rdj-swap-windows)

(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c C-v") 'revert-buffer)
(global-set-key (kbd "C-c C-w") 'rdj-toggle-whitespace-cleanup)
(global-set-key (kbd "C-c C-z") 'rdj-smartsize-frame)

(global-set-key (kbd "C-x f") 'find-file-in-project)

(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-/") 'hippie-expand) ;; overwrite the binding for dabbrev-expand, seems safer than defalias
(global-set-key (kbd "M-i") 'rdj-goto-symbol)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "C-M-|") 'align-regexp)

;; Tab should indent region if selected or this line if not
(defun rdj-maybe-tab ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (indent-region (region-beginning) (region-end) nil)
    (c-indent-command)))

(defun tab-indents-region () (local-set-key (kbd "TAB") 'rdj-maybe-tab))
(add-hook 'c-mode-common-hook 'tab-indents-region)

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

;; cua-mode is generally terrible, but every once in a while it is
;; handy to use because it lets you use normal region commands on
;; rectangular regions, which are selected with C-RET.
;;
;; So with this binding, I can toggle CUA mode on briefly and then
;; back off.
;;
(setq cua-enable-cua-keys nil)
(global-set-key (kbd "C-c r") 'cua-mode)

(provide 'rdj-bindings)
