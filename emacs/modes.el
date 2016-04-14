
;; ===================================================
;;   Modes, based on languages
;; ===================================================

;; Dash
(el-get-bundle dash)

;; Ruby
(el-get-bundle rbenv
  (global-rbenv-mode))
(el-get-bundle inf-ruby)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(el-get-bundle yari)
(el-get-bundle bundler)

;; Python
(defface python-version-color
  '((t (:weight bold :foreground "Blue")))
  "The face used to highlight the current python on the modeline.")
(defun pyenv--modeline-color (current-python)
  (append '(" [")
	  (list (propertize current-python 'face 'python-version-color))
	            '("]")))
(setq pyenv-modeline-function 'pyenv--modeline-color)
(el-get-bundle pyenv
  (global-pyenv-mode))

;; C
(el-get-bundle google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; JavaScript
(el-get-bundle js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(el-get-bundle json-mode)

;; vimrc
(el-get-bundle vimrc-mode)

;; Markdown
(el-get-bundle markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Yaml mode
(el-get-bundle yaml-mode)

;; Groovy mode
(el-get-bundle groovy-emacs-mode)

;; Org mode
(el-get-bundle org)
;; (el-get-bundle org-plus-contrib)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; Haml mode
(el-get-bundle haml-mode)
(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (define-key haml-mode-map "\C-m" 'newline-and-indent)))
