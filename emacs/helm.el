
;; ===================================================
;;   Helm
;; ===================================================

;; Install and load helm.el
(el-get-bundle helm)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages) ; man pages in helm

;; Dash documentation in Helm
(el-get-bundle helm-dash)
(setq helm-dash-browser-func 'eww)
(setq ruby-docs '("Ruby_2" "Ruby_on_Rails_3" "Ruby_on_Rails_4")
      js-docs   '("JavaScript" "jQuery")
)
(add-hook 'enh-ruby-mode-hook (lambda () (setq-local helm-dash-docsets ruby-docs)))

(global-set-key (kbd "C-c C-d") 'helm-dash)


;; Override original M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.'`'`
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Enable fuzzy matching
(setq helm-recentf-fuzzy-match    t
      helm-buffers-fuzzy-matching t
      helm-locate-fuzzy-match     t
      helm-M-x-fuzzy-match        t
      helm-semantic-fuzzy-match   t
      helm-imenu-fuzzy-match      t
      helm-apropos-fuzzy-match    t
      helm-lisp-fuzzy-completion  t)

;; Autoresize Helm
(helm-autoresize-mode 1)

(helm-mode 1)


;; Search
(when (executable-find "ack")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))



(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(global-set-key (kbd "C-x C-f") 'helm-find-files) ; find files with helm
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; choose from kill ring to yank
(global-set-key (kbd "C-x b") 'helm-mini) ; search from current buffer, recently opened files and create new buffer
(global-set-key (kbd "C-c h o") 'helm-occur) ; find the same patterns in current buffer

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))




;; ===================================================
;;   Helm extensions
;; ===================================================

;; helm-ls-git
(el-get-bundle helm-ls-git)

;; helm-descbinds
(el-get-bundle helm-descbinds)
(helm-descbinds-mode)

;; Helm-Projectile
(el-get-bundle helm-projectile)
(helm-projectile-on)

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

(global-set-key (kbd "C-x p f") 'helm-projectile-find-file) ; find files in project
(global-set-key (kbd "C-x p r") 'helm-projectile-recentf)   ; find recently opened files
