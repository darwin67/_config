
;; ===================================================
;;   Auto complete, Snippets
;; ===================================================

;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode 1)
  )
(el-get-bundle yasnippet-config)
(el-get-bundle yasnippet-snippets)

;; Auto-complete
(el-get-bundle auto-complete
  (ac-config-default)
)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'json-mode)
(add-to-list 'ac-modes 'yaml-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-use-fuzzy t)

;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode t)
)
