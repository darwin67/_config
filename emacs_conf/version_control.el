
;; ===================================================
;;   Version Control (git, github)
;; ===================================================

;; Magit, a git porcelain for emacs
(el-get-bundle magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log-all)
(global-set-key (kbd "C-c g b") 'magit-blame)

;; Git-gutter
(custom-set-variables
 '(git-gutter:update-interval 2))
(el-get-bundle git-gutter
  (global-git-gutter-mode t))

;; Git modes
(el-get-bundle git-modes)

;; Browse at remote (github, bitbucket)
(el-get-bundle browse-at-remote)
(global-set-key (kbd "C-c g g") 'browse-at-remote/browse)

;; Undo tree visualizer
(el-get-bundle undo-tree
  (global-undo-tree-mode))

;; Magit plugin for dealing with pull requests
(el-get-bundle magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
