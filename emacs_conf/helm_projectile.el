;; ===================================================
;;   Project management tools
;; ===================================================

;; Projectile, project management tool
(el-get-bundle projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Ignore these directories
(setq ignore-directories '(".git" "bundle" ".bundle" "tmp" ".sass_cache"
			   "modules" "log" "node_modules" ".node_modules"
			   "coverage" "vendor" "auto" "elpa" "$RECYCLE.BIN"
			   "calendars" "system" "sandbox" "backup" ".cask"
			   "eshell" "elpa"))
(setq projectile-globally-ignored-directories ignore-directories)

;; Ignore these files
(setq ignore-files '(".DS_Store" ".rspec" "tags" ".keep"))
(setq projectile-globally-ignored-files ignore-files)

;; Projectile-Rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)


;; ===================================================
;;   Helm
;; ===================================================

;; Install and load helm.el
(el-get-bundle helm)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages) ; man pages in helm

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

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action)             ; list actions using C-z

(global-set-key (kbd "C-x C-f") 'helm-find-files)     ; find files with helm
(global-set-key (kbd "M-y")     'helm-show-kill-ring) ; choose from kill ring to yank
(global-set-key (kbd "C-x b")   'helm-mini)           ; search from current buffer, recently opened files and create new buffer
(global-set-key (kbd "C-c o")   'helm-occur)          ; find the same patterns in current buffer
(global-set-key (kbd "C-c d")   'helm-descbinds)      ; descibe key bindings
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)   ; list buffers through helm

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))


;; ===================================================
;;   Helm extensions
;; ===================================================

;; helm-ls-git
(el-get-bundle helm-ls-git)
(global-set-key (kbd "C-c b p") 'helm-browse-project) ; Browse current project

;; helm-descbinds
(el-get-bundle helm-descbinds)
(helm-descbinds-mode)

;; Search (helm-ag, pt)
(el-get-bundle helm-ag)
(el-get-bundle pt)
(global-set-key (kbd "C-c p s p") 'projectile-pt) ; set key-binding for pt
;; (custom-set-variables ; set to use pt instead of ag
;;   '(helm-ag-base-command "pt -e --nocolor --nogroup"))

;; Helm-Projectile
(el-get-bundle helm-projectile)
(helm-projectile-on)

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

(global-set-key (kbd "C-x p f") 'helm-projectile-find-file) ; find files in project
(global-set-key (kbd "C-x p r") 'helm-projectile-recentf)   ; find recently opened files


;; gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 ;; helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(el-get-bundle helm-gtags)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(with-eval-after-load "helm-gtags"
  ;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))


;; ===================================================
;;   Dash document
;; ===================================================

(el-get-bundle dash-at-point)
(global-set-key (kbd "C-c s s") 'dash-at-point)
(global-set-key (kbd "C-c s d") 'dash-at-point-with-docset)
