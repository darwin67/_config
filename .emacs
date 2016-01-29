;; package.el
(require 'package)
(package-initialize)

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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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
;;; (transient-mark-mode t)

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
(global-set-key (kbd "C-c k")     'buf-move-up)
(global-set-key (kbd "C-c j")   'buf-move-down)
(global-set-key (kbd "C-c h")   'buf-move-left)
(global-set-key (kbd "C-c l")  'buf-move-right)

;; Allow additional setup when initializing el-get packages
(el-get-bundle with-eval-after-load-feature)

;; Monokai theme
(el-get-bundle monokai-theme
  (load-theme 'monokai t)
)

;; Smart parenthesis
(el-get-bundle smartparens
  (smartparens-global-mode 1)
)

;; Snippets
(el-get-bundle yasnippet
  (yas-global-mode 1)
)

;; Auto-complete
(el-get-bundle auto-complete
  (ac-config-default)
  (with-eval-after-load-feature auto-complete
    (add-to-list 'ac-modes 'text-mode)
    (add-to-list 'ac-modes 'fundamental-mode)
    (add-to-list 'ac-modes 'org-mode)
    (add-to-list 'ac-modes 'enh-ruby-mode)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")
    (setq ac-use-fuzzy t)
  )
)

;; Project browser, like NERDTree in vim
(el-get-bundle neotree
  (global-set-key (kbd "C-c C-n") 'neotree-toggle)
)


;; Ruby config
(el-get-bundle enh-ruby-mode)
(el-get-bundle inf-ruby)
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
