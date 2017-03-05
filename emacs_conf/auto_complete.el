
;; ===================================================
;;   Auto complete, Snippets
;; ===================================================

;; Company (complete anything) mode
(el-get-bundle company-mode
  (global-company-mode))

(el-get-bundle company-c-headers
  (add-to-list 'company-backends 'company-c-headers)) ; C
(el-get-bundle company-irony
  (add-to-list 'company-backends 'company-irony)) ; C/C++

(el-get-bundle go-company)              ; Go
(add-to-list 'company-backends 'company-go)

(el-get-bundle robe-mode)               ; Ruby
(add-to-list 'company-backends 'company-robe)

(el-get-bundle company-anaconda)        ; Python
(add-to-list 'company-backends 'company-anaconda)

(el-get-bundle company-edbi)            ; SQL
(add-to-list 'company-backends 'company-edbi)

(el-get-bundle company-tern)            ; Javascript
(add-to-list 'company-backends 'company-tern)

(el-get-bundle ac-company
  (add-to-list 'company-backends 'ac-company))

(with-eval-after-load "company"
  (progn
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous)))

(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match nil)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-preview-frontend))
  (define-key company-active-map [tab]
    'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB")
    'company-select-next-if-tooltip-visible-or-complete-selection))

(company-ac-setup)
(global-auto-complete-mode nil)


;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode t))


;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode 1))
(el-get-bundle yasnippet-config)
(el-get-bundle yasnippet-snippets)
