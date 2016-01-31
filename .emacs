
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

;; Encoding
(prefer-coding-system 'utf-8)

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

;; Remember the location of the previous cursor
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places")
)

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

;; Zone
;;   gets triggered after Emacs is idle for 5 minutes
(require 'zone)
(zone-when-idle (* 60 5))


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

;; Move text up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg)
	  (when (and (eval-when-compile
		       '(and (>= emacs-major-version 24)
			     (>= emacs-minor-version 3)))
		     (< arg 0))
	    (forward-line -1)))
	(forward-line -1))
      (move-to-column column t)))
  )
)

(defun move-text-down (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines down."
    (interactive "*p")
    (move-text-internal arg))

(defun move-text-up (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines up."
    (interactive "*p")
    (move-text-internal (- arg)))

(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)


;; ===================================================
;;   Plugins
;; ===================================================

;; Dash
(el-get-bundle dash)

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

;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode t)
)

;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode t)
)

;; Auto-complete

(el-get-bundle auto-complete
  (ac-config-default)
)
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
(el-get-bundle undo-tree
  (global-undo-tree-mode)
)

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
  (global-git-gutter-mode t)
)
(custom-set-variables
 '(git-gutter:update-interval 2)
)

;; Browse at remote (github, bitbucket)
(el-get-bundle browse-at-remote)
(global-set-key (kbd "C-c g g") 'browse-at-remote/browse)

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

;; Fly-check
;;  need to install texinfo in OS X
(el-get-bundle flycheck
  (global-flycheck-mode)
)
;; (el-get-bundle flycheck-color-mode-line)
;; (eval-after-load "flycheck"
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; )

;; Projectile, project management tool
(el-get-bundle projectile
  (projectile-global-mode)
)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

;; Projectile-Rails
(el-get-bundle projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; Web mode
(el-get-bundle web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; Ruby config
(el-get-bundle rbenv
  (global-rbenv-mode)
)
(el-get-bundle enh-ruby-mode)
(el-get-bundle inf-ruby)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(el-get-bundle yari)
(el-get-bundle bundler)

;; C config
(el-get-bundle google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; C++ config
(el-get-bundle flycheck-google-cpplint)
(defun cpp-check ()
  (require 'flycheck-google-cpplint)
  ;; Add Google C++ Style checker.
  ;; In default, syntax checked by Clang and Cppcheck.
  (flycheck-add-next-checker 'c/c++-cppcheck
			     '(warnings-only . c/c++-googlelint))
)
(add-hook 'c++-mode-hook 'cpp-check)

;; JavaScript
(el-get-bundle js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(el-get-bundle json-mode)

;; Common LISP
(el-get-bundle slime)
(el-get-bundle common-lisp-snippets)
