(defun rdj-ret-indents () (local-set-key (kbd "RET") 'newline-and-indent))
(defun rdj-no-auto-fill () (auto-fill-mode 0))

(defun rdj-setup-prog-mode-hook (hook)
  "Adds my default programmingish actions to the hook"
  (add-hook hook 'rdj-ret-indents)
  (add-hook hook 'rdj-no-auto-fill))

(rdj-setup-prog-mode-hook 'prog-mode-hook)

;; C/C++
(c-add-style "ryan" '("bsd"
  (c-basic-offset . 4)
  (c-offsets-alist
    (arglist-close . 0)
;    (objc-method-intro . 0)
;    (objc-method-args-cont . 0)
;    (objc-method-call-cont . +)
  )
))

(setq-default
  c-echo-syntactic-information-p 't
  c-default-style "ryan"
  c-auto-newline nil
  c-hungry-delete-key nil
)

;; C#
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(aput 'auto-mode-alist "\\.cs\\'" 'csharp-mode)

;; CSS
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(setq-default css-indent-offset 2)
(aput 'auto-mode-alist "\\.css\\'" 'css-mode)
(aput 'auto-mode-alist "\\.scss\\'" 'css-mode)
(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.scss"))
;; css-mode doesn't seem to run prog-mode-hook automatically
(rdj-setup-prog-mode-hook 'css-mode-hook)


;; Javascript
(autoload 'js2-mode "js2-mode" "Mode for editing javascript files" t)
(aput 'auto-mode-alist "\\.js\\'" 'js2-mode)
(setq js2-basic-offset 4
      js2-use-font-lock-faces t
      js2-auto-indent-flag nil
      js2-mirror-mode nil
      js2-mode-escape-quotes nil
      js2-rebind-eol-bol-keys nil)

;; LaTeX
(aput 'auto-mode-alist "\\.tex\\'" 'latex-mode)

;; Perl
(aput 'auto-mode-alist "\\.\\([pP][Llm]\\|t\\)\\'" 'cperl-mode)
(aput 'interpreter-mode-alist "perl"     'cperl-mode)
(aput 'interpreter-mode-alist "perl5"    'cperl-mode)
(aput 'interpreter-mode-alist "miniperl" 'cperl-mode)
(setq cperl-autoindent-on-semi t
      cperl-close-paren-offset -4
      cperl-continued-brace-offset -4
      cperl-continued-statement-offset 4
      cperl-extra-newline-before-brace t
      cperl-extra-newline-before-brace-multiline t
      cperl-font-lock t
      cperl-indent-comment-at-column-0 t
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-indent-region-fix-constructs nil
;;      cperl-indent-wrt-brace nil ;; what does this do?
      cperl-invalid-face nil
      cperl-label-offset 0                            ;; FOO:
      cperl-lineup-step 1                           ;; use cperl-indent-level
      cperl-merge-trailing-else nil
      cperl-min-label-indent 0                        ;; FOO:
      cperl-tab-always-indent t                       ;; regardless of line position
)

;; Turning off automatic abbrev-mode -- use C-' to expand
(add-hook 'cperl-mode-hook
          ;; Unfortunately, cperl-mode uses abbrev-mode to do it's electric
          ;; keyword expansion, and there's no way to make it not populate
          ;; those abbrevs (cperl-electric-keywords nil) just keeps cperl-mode
          ;; from turning on abbrev-mode.
          (function (lambda () (abbrev-mode 0))))

;; PHP
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(aput 'auto-mode-alist "\\.php\\'" 'php-mode)
(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.php"))

;; ruby
(add-hook 'ruby-mode-hook (lambda () (defun ruby-mode-set-encoding () nil)))
(aput 'auto-mode-alist "\\.builder\\'" 'ruby-mode)
(aput 'auto-mode-alist "\\.rake\\'" 'ruby-mode)
(eval-after-load 'ruby-mode '(require 'ruby-end))

;; shortcuts for .html.erb
(defun rdj-insert-eruby-encoded-section ()
  (interactive)
  (let ((pos (point)))
    (insert "<%=h  %>")
    (goto-char (+ pos 5))
))

(defun rdj-insert-eruby-literal-section ()
  (interactive)
  (let ((pos (point)))
    (insert "<%=  %>")
    (goto-char (+ pos 4))
))

(defun rdj-insert-eruby-code-section ()
  (interactive)
  (let ((pos (point)))
    (insert "<%  %>")
    (goto-char (+ pos 3))
))

(add-hook 'nxml-mode-hook (function (lambda () (progn
    (local-set-key "\C-c=" 'rdj-insert-eruby-encoded-section)
    (local-set-key "\C-c+" 'rdj-insert-eruby-literal-section)
    (local-set-key "\C-c%" 'rdj-insert-eruby-code-section)
))))

;; Text
(add-hook 'text-mode-hook (function (lambda () (flyspell-mode 1))))

;; YAML
(autoload 'yaml-mode "yaml-mode" nil t)
(aput 'auto-mode-alist "\\.ya?ml$" 'yaml-mode)

;; XCode Configuration
(aput 'auto-mode-alist "\\.xcconfig\\'" 'conf-mode)

;; XML
(aput 'auto-mode-alist "\\.xml\\'" 'nxml-mode)    ;; XML Document
(aput 'auto-mode-alist "\\.xsd\\'" 'nxml-mode)    ;; XML Schema
(aput 'auto-mode-alist "\\.plist\\'" 'nxml-mode)  ;; Apple plist file
(aput 'auto-mode-alist "\\.wxi\\'" 'nxml-mode)    ;; WiX installer
(aput 'auto-mode-alist "\\.wxs\\'" 'nxml-mode)    ;; WiX installer
(aput 'auto-mode-alist "\\.csproj\\'" 'nxml-mode) ;; VS C# project
(aput 'auto-mode-alist "\\.xaml\\'" 'nxml-mode)   ;; Microsoft XAML file

;; nXhtml
;; (load "nxhtml/autostart.el")
(add-hook 'nxml-mode-hook (function (lambda () (flyspell-prog-mode))))
 (setq
      nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      rng-nxml-auto-validate-flag nil)

;; HAML -- tweak nXhtml's majmodpri-mode-priorities or else it will sort .html.* above .haml and get the wrong mode
;(add-to-list 'majmodpri-mode-priorities 'haml-mode)
(autoload 'haml-mode "haml-mode" "Major mode for editing HAML code." t)
(aput 'auto-mode-alist "\\.haml\\'" 'haml-mode)
(add-hook 'haml-mode-hook (function (lambda () (flyspell-prog-mode))))
(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.haml"))


(provide 'rdj-prog)
