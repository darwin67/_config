
;; ===================================================
;;   Auto complete, Snippets
;; ===================================================

;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode 1))
(el-get-bundle yasnippet-config)
(el-get-bundle yasnippet-snippets)


;; Company (complete anything) mode
(el-get-bundle company-mode
  (global-company-mode))

(el-get-bundle company-c-headers)       ; C
(el-get-bundle company-irony)           ; C/C++
(el-get-bundle go-company)              ; Go
(el-get-bundle robe-mode)               ; Ruby
(el-get-bundle company-anaconda)        ; Python
(el-get-bundle company-edbi)            ; SQL
(el-get-bundle company-tern)            ; Javascript

(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-robe)
  (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-edbi)
  (add-to-list 'company-backends 'company-tern))


;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode t))
