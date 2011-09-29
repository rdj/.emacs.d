;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)
(defvar rdj-autosave-dir (concat user-emacs-directory "autosaves/"))
(make-directory rdj-autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "\\`#.*#\\'" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat rdj-autosave-dir (if buffer-file-name
                               (concat "#" (file-name-nondirectory buffer-file-name) "#")
                               (expand-file-name (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar rdj-backup-dir (concat user-emacs-directory "backups/"))
(setq backup-directory-alist (list (cons "." rdj-backup-dir)))

(provide 'rdj-turds)
