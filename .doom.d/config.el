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
;; (setq doom-theme 'doom-one)
(setq doom-theme 'zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; set cursor margin to 7 lines
(setq scroll-margin 7)

;; Auto rebalance windows after window actions like horiozntal or vertical splits
(setq window-combination-resize t)

;; Org mode settings
(setq
   org-bullets-bullet-list '("◉" "▶" "◆" "■")
   org-startup-truncated nil)

(global-company-mode)
(global-git-commit-mode t)
(editorconfig-mode t)
(drag-stuff-global-mode t)
(+global-word-wrap-mode t)
(global-tree-sitter-mode)               ; https://emacs-tree-sitter.github.io/getting-started/

;; ===================================================================
;; Key Bindings

;; Global
(map!
 ;; Vim like 'o' and 'O' behaviours
 "C-o" 'open-next-line
 "M-o" 'open-previous-line

 ;; Set bindings for window splits that are more intuitive
 (:prefix "C-x"
   "-" 'split-window-below
   "|" 'split-window-right
   "g" 'magit-status))

;; Leader prefix
(map! :leader
      ;; toggle code folding
      :desc "fold toggle" "C-f" '+fold/toggle

      ;; Jumps
      :desc "Jump to definition" "]" '+lookup/definition
      :desc "Jump back" "[" 'better-jumper-jump-backward

      ;; Swap buffers
      :desc "buffer" "b" nil
      (:prefix "b"
        :desc "Move buffer up" "k" 'buf-move-up
        :desc "Move buffer down" "j" 'buf-move-down
        :desc "Move buffer left" "h" 'buf-move-left
        :desc "Move buffer right" "l" 'buf-move-right
        :desc "Show imenu" "i" 'lsp-ui-imenu))

;; ===================================================================
;; Language settings

;; Elixir

;; Create a buffer-local hook to run elixir-format on save
(add-hook 'elixir-mode-hook
          (lambda() (add-hook 'before-save-hook 'elixir-format nil t)))

;; ===================================================================
;; Others

;; LSP mode
(setq
 gc-cons-threshold #x8000000
 read-process-output-max (* 10000 1024) ;; 10mb
 lsp-completion-provider :capf
 lsp-idle-delay 0.2
 lsp-auto-configure t
 lsp-enable-file-watchers nil
 lsp-lens-place-position 'above-line
 lsp-ui-doc-show-with-mouse nil
 lsp-headerline-breadcrumb-enable t
 lsp-ui-sideline-enable nil
 lsp-eldoc-enable-hover nil
 lsp-completion-show-detail t)

;; Load Emacs as full screen on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Rust
(setq
 rustic-lsp-server 'rust-analyzer
 lsp-rust-all-features t
 lsp-rust-analyzer-exclude-globs ["tmp/**/*"]
 lsp-enable-file-watchers nil
 rustic-flycheck-clippy-params "--message-format=json"
 )

(add-to-list '+word-wrap-disabled-modes 'rust-mode)
(add-to-list '+word-wrap-disabled-modes 'rustic-mode)

;; (push 'rustic-clippy flycheck-checkers)
;; (remove-hook 'rustic-mode-hook 'flycheck-mode)

;; systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; Liquid
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))

;; beancount (plain text accounting)
;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook #'outline-minor-mode)

;; web-mode
(setq web-mode-markup-indent-offset 2)
