
;; ===================================================
;;   Modes, based on languages
;; ===================================================

;; Dash
(el-get-bundle dash)


;; ===================================================
;; Ruby
(el-get-bundle inf-ruby)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(el-get-bundle yari)
(add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))


;; ===================================================
;; Python
(el-get-bundle python-mode)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")

(el-get-bundle elpy
  (elpy-enable))
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(el-get-bundle py-autopep8
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


;; ===================================================
;; C / C++
(el-get-bundle function-args
  (fa-config-default))
(set-default 'semantic-case-fold t)
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(el-get-bundle google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; ===================================================
;; JavaScript
(el-get-bundle js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . js2-mode))
(el-get-bundle json-mode)


;; ===================================================
;; Markdown
(el-get-bundle markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ===================================================
;; Yaml mode
(el-get-bundle yaml-mode)


;; ===================================================
;; Org mode
(el-get-bundle org)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))


;; ===================================================
;; Haml mode
;; (el-get-bundle haml-mode)
;; (add-hook 'haml-mode-hook
;; 	  (lambda ()
;; 	    (setq indent-tabs-mode nil)
;; 	    (define-key haml-mode-map "\C-m" 'newline-and-indent)))


;; ===================================================
;; Dockerfile mode
(el-get-bundle dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


;; ===================================================
;; Nginx mode
(el-get-bundle nginx-mode)
(add-to-list 'auto-mode-alist '("\\.nginx\\'" . nginx-mode))


;; ===================================================
;; Systemd mode
(el-get-bundle systemd-mode)


;; ===================================================
;; nasm mode
(el-get-bundle nasm-mode)
(add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
