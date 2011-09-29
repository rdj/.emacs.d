(setq ido-enable-flex-matching 't
      ido-save-directory-list-file (concat user-emacs-directory ".ido.last")
)

(ido-mode 1)

(provide 'rdj-ido)
