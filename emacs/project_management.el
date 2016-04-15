
;; ===================================================
;;   Project management tools
;; ===================================================

;; Projectile, project management tool
(el-get-bundle projectile
  (projectile-global-mode))
(setq projectile-enable-caching t)

;; Ignore these directories
(setq ignore-directories '(".git" "bundle" ".bundle" "tmp" ".sass_cache"
			   "modules" "log" "node_modules" ".node_modules"
			   "coverage" "vendor" "auto" "elpa" "$RECYCLE.BIN"
			   "calendars" "system" "sandbox" "backup" ".cask"
			   "eshell" "elpa"))
(setq projectile-globally-ignored-directories ignore-directories)

;; change root automatically when project has changed
(setq projectile-switch-project-action 'neotree-projectile-action)

;; Ignore these files
(setq ignore-files '(".DS_Store" ".rspec" "tags" ".keep"))
(setq projectile-globally-ignored-files ignore-files)

;; Projectile-Rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
