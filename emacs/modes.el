
;; ===================================================
;;   Modes, based on languages
;; ===================================================

;; Dash
(el-get-bundle dash)

;; Web
(el-get-bundle web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

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

;; Common LISP
(el-get-bundle slime)
;; (el-get-bundle common-lisp-snippets)

;; vimrc
(el-get-bundle vimrc-mode)

;; Markdown
(el-get-bundle markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Twitter
(el-get-bundle twittering-mode)
(setq twittering-use-master-password t) ; use master password
(setq twittering-icon-mode t) ; enable icon
(setq twittering-timer-interval (* 60 5)) ; update every 5 minutes
(setq twittering-url-show-status nil)
(setq twittering-use-icon-storage t) ; use icons from local storage
(setq twittering-icon-storage-limit t) ; store icons in local storage
(setq twittering-display-remaining t) ; display remaining API calls
(add-hook 'twittering-edit-mode-hook ; spell check for tweets
	  (lambda ()
	    (ispell-minor-mode)
	    (flyspell-mode)
	    (set-face-bold-p 'twittering-username-face t)
	    (set-face-foreground 'twittering-username-face "DeepSkyBlue3")
	    (set-face-foreground 'twittering-uri-face "gray60")
	    (setq twittering-status-format "%i %p%s / %S:n%FOLD{%T}n%r %R [%@]")
	    (setq twittering-retweet-format " RT @%s: %t")
	    (define-key twittering-mode-map (kbd "F") 'twittering-favorite)
	    (define-key twittering-mode-map (kbd "R") 'twittering-native-retweet)
	    (define-key twittering-mode-map (kbd "<") (lambda () (interactive) (goto-char (point-min))))
	    (define-key twittering-mode-map (kbd ">") (lambda () (interactive) (goto-char (point-max))))
	  )
)

;; Yaml mode
(el-get-bundle yaml-mode)

;; Groovy mode
(el-get-bundle groovy-emacs-mode)

;; Org mode
(el-get-bundle org)
;; (el-get-bundle org-plus-contrib)

;; Haml mode
(el-get-bundle haml-mode)
(add-hook 'haml-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (define-key haml-mode-map "\C-m" 'newline-and-indent)))
