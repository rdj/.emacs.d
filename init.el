;; On Mac OS X, GUI programs do not get a sensible PATH variable (i.e.
;; from /usr/libexec/path_helper). You can mitigate this by setting
;; the PATH variable in the binary plist file at this path:
;;
;;   ~/.MacOSX/environment.plist
;;
;; Easiest way is using defaults:
;;
;;   defaults write $HOME/.MacOSX/environment PATH "$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin"
;;
;; You will have to log out and back in for this to take effect.

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
(require 'rdj-early-off)
(require 'rdj-package)
(require 'rdj-ido)
(require 'rdj-smartsize)
(require 'rdj-turds)
(require 'rdj-whitespace)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'rdj-bindings)
(require 'rdj-theme)
(require 'rdj-state)

