
;; ===================================================
;;   Version Control (git, github)
;; ===================================================

;; Magit, a git porcelain for emacs
(el-get-bundle magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g c") 'magit-commit)
(global-set-key (kbd "C-c g d") 'magit-diff)
(global-set-key (kbd "C-c g l") 'magit-log-all)
(global-set-key (kbd "C-c g p") 'magit-push)
(global-set-key (kbd "C-c g b") 'magit-blame)

;; Git-gutter
(el-get-bundle git-gutter
  (global-git-gutter-mode t))
(custom-set-variables
 '(git-gutter:update-interval 2))

;; Git modes
(el-get-bundle git-modes)

;; Browse at remote (github, bitbucket)
(el-get-bundle browse-at-remote)
(global-set-key (kbd "C-c g g") 'browse-at-remote/browse)

;; Undo tree visualizer
(el-get-bundle undo-tree
  (global-undo-tree-mode))
