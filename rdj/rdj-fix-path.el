;; rdj-fix-path.el --- Ryan D Johnson's Emacs configuration

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

;; On Mac, when launching Emacs through Finder it does not get a
;; reasonable PATH/MANPATH as you may have customized systemwise using
;; /etc/paths.d and /etc/manpaths.d; nor does it get any
;; customizations you may have made in your shell init files, e.g.
;; adding ~/bin or what have you.

;; This makes it a pain to invoke non-system binaries from emacs, e.g.
;; aspell installed through homebrew.

;; So https://github.com/purcell/exec-path-from-shell invokes a real
;; shell and extracts the variables from it.

;; A useful hack.

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(provide 'rdj-fix-path)
