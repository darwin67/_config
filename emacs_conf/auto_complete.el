
;; ===================================================
;;   Auto complete, Snippets
;; ===================================================

;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode 1))
(el-get-bundle yasnippet-config)
(el-get-bundle yasnippet-snippets)

;; Auto-complete
(el-get-bundle auto-complete
  (ac-config-default))

;; Text
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)

;; Web
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'css-mode)

;; C, C++
(add-to-list 'ac-modes 'c-mode)
(add-to-list 'ac-modes 'c++-mode)

;; Ruby
(add-to-list 'ac-modes 'ruby-mode)

;; Python
(add-to-list 'ac-modes 'python-mode)

;; Javascript
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'json-mode)

;; Yaml
(add-to-list 'ac-modes 'yaml-mode)

;; Java
(add-to-list 'ac-modes 'groovy-emacs-mode)

;; Vim
(add-to-list 'ac-modes 'vimrc-mode)

;; Markdown
(add-to-list 'ac-modes 'markdown-mode)

;; Go
(add-to-list 'ac-modes 'go-mode)

;; Shell
(add-to-list 'ac-modes 'shell-mode)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-use-fuzzy t)

;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode t))
