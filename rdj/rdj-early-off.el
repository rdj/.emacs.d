(setq inhibit-startup-message 't)
(setq initial-scratch-message nil)

(menu-bar-mode (if rdj-is-mac 1 -1))
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'rdj-early-off)
