
;; ===================================================
;;   Theme
;; ===================================================

;; Monokai theme
(el-get-bundle monokai-theme
  (load-theme 'monokai t)
)

;; Powerline
(el-get-bundle powerline
  (powerline-default-theme)
)

;; Spaceline
;;  This doesn't work on iTerm2 or Mac OS
;; (el-get-bundle s)
;; (el-get-bundle eyebrowse)
;; (el-get-bundle persp-mode)
;; (el-get-bundle window-numbering)
;; (el-get-bundle evil)
;; (el-get-bundle spaceline)
;; (require 'spaceline-config)
;; (setq powerline-default-separator 'wave)
;; (spaceline-emacs-theme)
