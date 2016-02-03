
;; ===================================================
;;   Basic setup
;; ===================================================

;; Encoding
(prefer-coding-system 'utf-8)

;; Remove tool bar, menu bar and scroll bar
(unless (boundp tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
;; (unless (boundp scroll-bar-mode) (scroll-bar-mode -1))

;; ido
(ido-mode t)
(ido-everywhere t)

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
(global-hl-line-mode 1)
;; (set-face-background 'hl-line "#3e4446")
;; (set-face-foreground 'hl-line nil)
(custom-set-faces
 '(hl-line ((t (:background "color-236"))))
)

;; Parenthesis
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(transient-mark-mode t)

;; Backup
(setq make-backup-files nil)
;; (setq backup_dir
;;       (let ((config_dir (getenv "CONFIG")))
;; 	(concat config_dir "/tmp/.saves"))
;; )
;; (setq backup-directory-alist `(("." . backup_dir))
;;       backup-by-copying t
;;       delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t
;; )

;; Delete auto save files
(setq delete-auto-save-files t)

;; Split windows
(global-set-key (kbd "C-x -") 'split-window-below) ; horizontal
(global-set-key (kbd "C-x |") 'split-window-right) ; vertical

;; Zone
;;   gets triggered after Emacs is idle for 5 minutes
(require 'zone)
(zone-when-idle (* 60 5))
