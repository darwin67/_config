
;; ===================================================
;;   Project management tools
;; ===================================================

;; Fuzzy finder
(el-get-bundle flx
  (flx-ido-mode t)
)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(el-get-bundle fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "bundle" "modules" "tmp"))
			    (files ("*.jpg" "*.png" "*.zip" "*~")))
)

;; Projectile, project management tool
(el-get-bundle projectile
  (projectile-global-mode)
)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

;; Projectile-Rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
