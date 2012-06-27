;; At some point a Lion update broke loading PATH from
;; environment.plist. Thanks to jwiegley for this little elisp gem to
;; load the file manually.
;;
;; Source: https://github.com/jwiegley/dot-emacs/blob/master/init.el

(when rdj-is-mac
  (let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
    (when (file-readable-p plist)
      (let ((dict (cdr (assq 'dict (cdar (xml-parse-file plist))))))
        (while dict
          (if (and (listp (car dict))
                   (eq 'key (caar dict)))
              (setenv (car (cddr (car dict)))
                      (car (cddr (car (cddr dict))))))
          (setq dict (cdr dict))))

      ;; Configure exec-path based on the new PATH
      (setq exec-path nil)
      (mapc (apply-partially #'add-to-list 'exec-path)
            (nreverse (split-string (getenv "PATH") ":"))))))

(provide 'rdj-fix-path)

