;; rdj-package.el --- Ryan D Johnson's Emacs configuration

;; Copyright (C) 1998-2015 Ryan D Johnson

;; Author: Ryan D Johnson <ryandjohnson@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;; Bootstrap package.el
(require 'package)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq rdj-packages-refreshed nil)
(when (not package-archive-contents)
  (package-refresh-contents)
  (setq rdj-refreshed-package 't))

;; Add in your own as you wish:
(defvar rdj-packages
  '(
    use-package
    auctex
    ag
    coffee-mode
    csharp-mode
    exec-path-from-shell
    flx-ido
    ;groovy-mode
    haml-mode
    helm
    helm-ag
    helm-projectile
    magit
    php-mode
    plantuml-mode
    projectile
    python-mode
    py-autopep8
    racket-mode
    ruby-electric
    rust-mode
    rustic
    smex
    swift-mode
    unicode-fonts
    yaml-mode
    color-theme-sanityinc-tomorrow
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p rdj-packages)
  (when (not (package-installed-p p))
    (when (not rdj-packages-refreshed)
      (package-refresh-contents)
      (setq rdj-packages-refreshed 't))
    (package-install p)))

(provide 'rdj-package)
