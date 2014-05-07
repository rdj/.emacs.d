(defun rdj-ret-indents () (local-set-key (kbd "RET") 'newline-and-indent))
(defun rdj-no-auto-fill () (auto-fill-mode 0))
(defun rdj-sub-words () (subword-mode 1))

(defun rdj-setup-prog-mode-hook (hook) ;; Some programming modes don't run prog-mode-hook
  "Adds my default programmingish actions to the hook"
  (add-hook hook 'rdj-ret-indents)
  (add-hook hook 'rdj-no-auto-fill)
  (add-hook hook 'rdj-sub-words)
  )

(defun rdj-add-to-ffip (pattern)
  "Delay-adds a filename pattern to find-file-in-project"
  (eval-after-load 'find-file-in-project
    `(add-to-list 'ffip-patterns ,pattern)))

(setq ffip-limit 1024)

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
(rdj-add-to-ffip "*.h")
(rdj-add-to-ffip "*.c")
(rdj-add-to-ffip "*.cpp")
(rdj-add-to-ffip "*.cxx")
(rdj-add-to-ffip "*.m")
(rdj-add-to-ffip "*.java")

(setq-default
  c-echo-syntactic-information-p 't
  c-default-style "ryan"
  c-auto-newline nil
  c-hungry-delete-key nil
)
(rdj-setup-prog-mode-hook 'c-mode-common-hook) ;; vendored cc-mode isn't a prog-mode derived mode

;; C#
(autoload 'csharp-mode "csharp-mode-0.8.5" "Major mode for editing C# code." t)
(aput 'auto-mode-alist "\\.cs\\'" 'csharp-mode)
(rdj-add-to-ffip "*.cs")

;; CSS
(setq-default css-indent-offset 2)
(aput 'auto-mode-alist "\\.scss\\'" 'css-mode)
(rdj-add-to-ffip "*.scss")
(rdj-setup-prog-mode-hook 'css-mode-hook) ;; css-mode doesn't run prog-mode-hook

;; LaTeX
(aput 'auto-mode-alist "\\.tex\\'" 'latex-mode)

;; Perl
(aput 'auto-mode-alist "\\.\\([pP][Llm]\\|t\\)\\'" 'cperl-mode)
(rdj-add-to-ffip "*.pm")
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
;(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(aput 'auto-mode-alist "\\.php\\'" 'php-mode)
(rdj-add-to-ffip "*.php")

;; ruby
(add-hook 'ruby-mode-hook (lambda () (defun ruby-mode-set-encoding () nil)))
(rdj-setup-prog-mode-hook 'ruby-mode-hook)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(rdj-add-to-ffip "*.rake")
(rdj-add-to-ffip "*.erb")

;; ruby-insert-end was removed from ruby-mode.el, see https://groups.google.com/group/emacs-on-rails/msg/565fba8263233c28
(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode)))
(autoload 'ruby-electric-mode "ruby-electric" nil t)
(setq ruby-electric-expand-delimiters-list '())

;; Text
(add-hook 'text-mode-hook (function (lambda () (flyspell-mode 1))))
(add-hook 'org-mode-hook (function (lambda () (flyspell-mode -1))))

;; YAML
(autoload 'yaml-mode "yaml-mode" nil t)
(aput 'auto-mode-alist "\\.ya?ml$" 'yaml-mode)
(rdj-add-to-ffip "*.yaml")
(rdj-add-to-ffip "*.yml")

;; XCode Configuration
(aput 'auto-mode-alist "\\.xcconfig\\'" 'conf-mode)

;; XML
(aput 'auto-mode-alist "\\.xml\\'"    'nxml-mode) ;; XML Document
(aput 'auto-mode-alist "\\.xsd\\'"    'nxml-mode) ;; XML Schema
(aput 'auto-mode-alist "\\.plist\\'"  'nxml-mode) ;; Apple plist file
(aput 'auto-mode-alist "\\.wxi\\'"    'nxml-mode) ;; WiX installer
(aput 'auto-mode-alist "\\.wxs\\'"    'nxml-mode) ;; WiX installer
(aput 'auto-mode-alist "\\.csproj\\'" 'nxml-mode) ;; VS C# project
(aput 'auto-mode-alist "\\.xaml\\'"   'nxml-mode) ;; Microsoft XAML file
(rdj-add-to-ffip "*.xml")
(rdj-add-to-ffip "*.xsd")
(rdj-add-to-ffip "*.plist")
(rdj-add-to-ffip "*.wxi")
(rdj-add-to-ffip "*.wxs")
(rdj-add-to-ffip "*.csproj")
(rdj-add-to-ffip "*.xaml")

;; HAML
;; This is vendored instead of from ELPA because I needed to apply a patch for emacs 24
;; https://github.com/thorstadt/haml-mode/commit/cf5beeda8d6ea7e021d5994eee7c4be45b695964
(autoload 'haml-mode "haml-mode" nil t)
(aput 'auto-mode-alist "\\.haml" 'haml-mode)
(rdj-setup-prog-mode-hook 'haml-mode-hook)
(add-hook 'haml-mode-hook (function (lambda () (flyspell-prog-mode))))
(rdj-add-to-ffip "*.haml")

;; CoffeeScript
(setq-default coffee-tab-width 2)
(rdj-add-to-ffip "*.coffee")

(projectile-global-mode)

(provide 'rdj-prog)
