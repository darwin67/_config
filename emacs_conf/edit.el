
;; ===================================================
;;   Editing
;; ===================================================

;; Multiple cursors
(el-get-bundle! multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; Text folding
(el-get-bundle s) ; dependency
(el-get-bundle origami
  (global-origami-mode))
(global-set-key (kbd "C-c f") 'origami-toggle-node)
(global-set-key (kbd "C-c C-f") 'origami-toggle-all-nodes)
