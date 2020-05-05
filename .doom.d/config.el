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
;; (setq doom-theme 'zenburn)

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

;; Org mode settings
(setq
   org-bullets-bullet-list '("◉" "▶" "◆" "■")
   org-startup-truncated nil)

(global-company-mode)
(global-git-commit-mode t)
(editorconfig-mode t)
(drag-stuff-global-mode t)
(+global-word-wrap-mode t)


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
   "|" 'split-window-right))

;; Leader prefix
(map! :leader
      ;; Change prefix maps for lookup and versioning
      "v" nil
      :desc "lookup" "l" doom-leader-lookup-map
      :desc "version control" "g" doom-leader-versioning-map

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
        :desc "Move buffer right" "l" 'buf-move-right))

;; ===================================================================
;; Language settings

;; Elixir
(setq lsp-clients-elixir-server-executable "~/.lsp/elixir-ls/language_server.sh")
;; Create a buffer-local hook to run elixir-format on save
(add-hook 'elixir-mode-hook
          (lambda() (add-hook 'before-save-hook 'elixir-format nil t)))

;; Additional YAML extensions
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; ===================================================================
;; Others

;; Load Emacs as full screen on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
