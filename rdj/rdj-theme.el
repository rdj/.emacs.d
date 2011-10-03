;; The variable redisplay-dont-pause, when set to t, will cause Emacs
;; to fully redraw the display before it processes queued input
;; events. This may have slight performance implications if you’re
;; aggressively mouse scrolling a document or rely on your keyboard’s
;; auto repeat feature. For most of us, myself included, it’s probably
;; a no-brainer to switch it on.
(setq redisplay-dont-pause t)

;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Highlight paren expressions
(show-paren-mode 1)
(setq show-paren-style 'paren)

;; Show file position
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

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
