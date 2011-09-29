;; Not included on purpose, but didn't want to lose this

;; Cut/paste
(when window-system
;; Paste where I am, not where I click
  (setq-default mouse-yank-at-point 't)
;; Kill/yank should use the WM clipboard ...
  (setq-default x-select-enable-clipboard 't)
;; ... but shouldn't mess with the X 'primary selection' middle click deal
  (setq-default x-select-enable-primary nil))

(defun rdj-disable-x-clipboard ()
  "Turns off the OS clipboard; useful for speeding up slow remote X Windows connections."
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(provide 'rdj-x-windows)
