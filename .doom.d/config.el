;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Darwin D. Wu"
      user-mail-address "wuddarwin@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; set cursor margin to 7 lines
(setq scroll-margin 7)

;; Auto rebalance windows after window actions like horiozntal or vertical splits
(setq window-combination-resize t)

;; Change leader key to M-m in Emacs (non Evil user)
(setq
 doom-leader-alt-key "M-m"
 doom-localleader-alt-key "C-c m"
 )

;; Org mode settings
(setq
   org-bullets-bullet-list '("◉" "▶" "◆" "■")
   org-startup-truncated nil)

(global-company-mode)
(global-git-commit-mode t)
(editorconfig-mode t)

;; ===================================================================
;; Key Bindings
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)

;; (global-set-key (kbd "C-c ]") 'dumb-jump-go)
;; (global-set-key (kbd "C-c C-]") 'spacemacs/jump-to-definition)
;; (global-set-key (kbd "C-c }") 'spacemacs/jump-to-definition-other-window)
;; (global-set-key (kbd "C-c [") 'dumb-jump-back)
;; (global-set-key (kbd "C-c \\") 'dumb-jump-quick-look)

;; Multiple cursors
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; Text folding
(global-set-key (kbd "C-c f") 'origami-toggle-node)
(global-set-key (kbd "C-c C-f") 'origami-toggle-all-nodes)

;; Swap buffers
(global-set-key (kbd "C-c k") 'buf-move-up)
(global-set-key (kbd "C-c j") 'buf-move-down)
(global-set-key (kbd "C-c h") 'buf-move-left)
(global-set-key (kbd "C-c l") 'buf-move-right)

;; move text up and down
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; Vim like 'o' and 'O' behaviours
(defvar newline-and-indent t
  "Modify the behaviour of the open-*-line functions to cause them to autoindent.")
(defun open-next-line (arg)
  "Move to the end of line and then opens a new line"
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent (indent-according-to-mode)))
(defun open-previous-line (arg)
  "Open a new line before the current one."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)


;; Load Emacs as full screen on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
