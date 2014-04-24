;; Bootstrap package.el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq rdj-packages-refreshed nil)
(when (not package-archive-contents)
  (package-refresh-contents)
  (setq rdj-refreshed-package 't))

;; Add in your own as you wish:
(defvar rdj-packages
  '(
    coffee-mode
    flx-ido
    helm
    helm-projectile
    magit
    php-mode
    projectile
    ruby-electric
    smex
    unicode-fonts
    yaml-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p rdj-packages)
  (when (not (package-installed-p p))
    (when (not rdj-packages-refreshed)
      (package-refresh-contents)
      (setq rdj-packages-refreshed 't))
    (package-install p)))

(provide 'rdj-package)
