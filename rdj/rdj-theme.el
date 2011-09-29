;; Highlight paren expressions
(show-paren-mode 1)
(setq show-paren-style 'paren)

;; Show file position
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(global-hl-line-mode t) ;; trying this out for now

;; Window title
(defun rdj-shorten-filename (filename)
  (replace-regexp-in-string (format "^%s" (getenv "HOME")) "~" filename))
(setq frame-title-format (list "%b"
                               (list 'buffer-file-name
                                     (list " : " '(:eval (rdj-shorten-filename buffer-file-name))))
                               (list 'dired-directory
                                     (list " : " '(:eval (rdj-shorten-filename dired-directory))))))


;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn)

(provide 'rdj-theme)
