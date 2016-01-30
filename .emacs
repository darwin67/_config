;; ===================================================
;;   Package management
;; ===================================================

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; ===================================================
;;   Basic setup
;; ===================================================

;; ido
(ido-mode t)
(ido-everywhere t)

(prefer-coding-system 'utf-8)

;; Relaod emacs
(defun reload-emacs ()
  "Reload .emacs at the home directory."
  (interactive)
  (load-file "~/.emacs")
)
(global-set-key (kbd "C-c C-r") 'reload-emacs)

;; History
(setq history-length 10000)

;; Save mini-buffer
(savehist-mode 1)

;; Hightlight line, color grey
(global-hl-line-mode t)
(custom-set-faces
 '(hl-line ((t (:background "color-236"))))
)

;; Parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(transient-mark-mode t)

;; No backup
(setq make-backup-file nil)

;; Delete auto save files
(setq delete-auto-save-files t)

;; Split windows
(global-set-key (kbd "C-x -") 'split-window-below) ; horizontal
(global-set-key (kbd "C-x |") 'split-window-right) ; vertical


;; ===================================================
;;   Navigation
;; ===================================================

;; Wind move (http://www.emacswiki.org/emacs/WindMove)
;;   Using Ctr-c <Arrow> to move between buffers instead of typing Ctr-x o
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Buffer move for lazy people like me
(el-get-bundle buffer-move)
(global-set-key (kbd "C-c k") 'buf-move-up)
(global-set-key (kbd "C-c j") 'buf-move-down)
(global-set-key (kbd "C-c h") 'buf-move-left)
(global-set-key (kbd "C-c l") 'buf-move-right)

;; Tab management
(defun previous-frame ()
  "A function to go to the previous frame."
  (interactive)
  (other-frame -1)
)
(global-set-key (kbd "C-x t n") 'make-frame-command)
(global-set-key (kbd "C-x n t") 'other-frame)
;; (global-set-key (kbd "C-x p t") 'previous-frame)


;; ===================================================
;;   Plugins
;; ===================================================

;; Monokai theme
(el-get-bundle monokai-theme)
(load-theme 'monokai t)

;; Powerline
(el-get-bundle powerline)
(powerline-default-theme)

;; Spaceline
;; (el-get-bundle spaceline)
;; (add-to-list 'load-path "~/.emacs/el-get/dash")
;; (add-to-list 'load-path "~/.emacs/el-get/spaceline")
;; (require 'spaceline-config)
;; (spaceline-emacs-theme)

;; Smart parenthesis
(el-get-bundle smartparens)
(smartparens-global-mode t)

;; Snippets
(el-get-bundle yasnippet)
(yas-global-mode t)

;; Auto-complete
(el-get-bundle auto-complete)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'json-mode)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-use-fuzzy t)

;; Project browser, like NERDTree in vim
(el-get-bundle neotree)
(global-set-key (kbd "C-c n t") 'neotree-toggle)

;; Multiple cursors
(el-get-bundle! multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; Undo tree visualizer
(el-get-bundle undo-tree)
(global-undo-tree-mode)

;; Magit, a git porcelain for emacs
(el-get-bundle magit)

;; Git-gutter
(el-get-bundle git-gutter)
(global-git-gutter-mode t)
(custom-set-variables
 '(git-gutter:update-interval 2))

;; Fuzzy finder
(el-get-bundle flx)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq gc-cons-threshold 20000000)

(el-get-bundle fiplr)
(setq fiplr-ignored-globs '((directories (".git" ".svn" "bundle" "modules" "tmp"))
			    (files ("*.jpg" "*.png" "*.zip" "*~"))))

;; Projectile, project management tool
(el-get-bundle projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

;; Projectile-Rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Ruby config
(el-get-bundle rbenv
  (global-rbenv-mode)
)
(el-get-bundle enh-ruby-mode)
(el-get-bundle inf-ruby
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
)
(el-get-bundle yari)
(el-get-bundle bundler)

;; C, C++ config
(el-get-bundle irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; JavaScript
(el-get-bundle js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(el-get-bundle json-mode)
