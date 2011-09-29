(require 'cl)
(require 'assoc)

;; My files are in .emacs.d/rdj
(setq rdj-elisp-path (concat user-emacs-directory "rdj/"))
(push rdj-elisp-path load-path)

;; Customize is basically an anti-feature, but some things like theme
;; whitelists according to file hashes are just probably going to live
;; there.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(require 'rdj-defs)
(require 'rdj-package)
(require 'rdj-mac)
(require 'rdj-smartsize)
(require 'rdj-turds)
(require 'rdj-whitespace)

(require 'rdj-theme)

(require 'rdj-state)

