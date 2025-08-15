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
(setq doom-font (font-spec :family "monospace" :size 11))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq
 deft-directory "~/notes/scratch"
 org-directory "~/notes/scratch"
 org-agenda-files (directory-files-recursively "~/notes/scratch" "\\.org$")
 org-archive-location "~/notes/scratch/archives/%s_archive::"
 org-startup-folded "showeverything"
 org-tag-alist '(("@code" . ?c))
 org-hide-leading-stars t
 org-blank-before-new-entry t
 org-startup-with-inline-images t
 org-image-actual-width nil
 org-log-done 'note
 org-bullets-bullet-list '("◉" "○" "✸" "►" ">" ">>")
 org-startup-truncated nil

 org-emphasis-alist
 '(("*" (bold :foreground "orange" ))
   ("/" italic)
   ("_" underline)
   ("=" (:background "maroon" :foreground "white"))
   ("+" (:background "deep sky blue" :foreground "white"))
   ("~" (:strike-through t)))
 )

(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook 'org-rainbow-tags-mode)
(add-hook 'org-mode-hook (lambda() (setq display-line-numbers-type nil)))

;; Show line numbers only on the focused file
(defun display-line-numbers-current-buffer ()
  "Enable line numbers in the currently focused buffer, and disable it in all other buffers."
  (when (and (equal (selected-frame) (window-frame))
             (not (frame-parent)))
    (let ((buffer-list (buffer-list)))
      (dolist (buf buffer-list)
        (with-current-buffer buf
          (when (and (not (eq (current-buffer) (window-buffer)))
                     (display-line-numbers-mode))
            (display-line-numbers-mode -1)))
        (with-current-buffer (window-buffer)
          (when (eq buf (current-buffer))
            (unless (display-line-numbers-mode)
              (display-line-numbers-mode 1))))))))

(add-hook 'buffer-list-update-hook #'display-line-numbers-current-buffer)

(setq-default
 indent-tabs-mode nil
 tab-width 2)
(setq
 ;; set cursor margin to 7 lines
 scroll-margin 7
 ;; Auto rebalance windows after window actions like horiozntal or vertical splits
 window-combination-resize t

 ;; corfu - code completion
 corfu-preview-current t
 )

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
          "|" 'split-window-right
          "g" 'magit-status

          ;; Move windows with arrows
          "<up>" 'windmove-up
          "<down>" 'windmove-down
          "<right>" 'windmove-right
          "<left>" 'windmove-left))

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
       :desc "Move buffer right" "l" 'buf-move-right))

;; Disable formatting on these modes
(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        tex-mode         ; latexindent is broken
        latex-mode
        typescript-mode
        web-mode))

;; ===================================================================
;; Language settings

;; Elixir

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode) . ("lexical"))))

;; Create a buffer-local hook to run elixir-format on save
;;
;; NOTE: embedded into elixir-ts-mode
;; (add-hook 'elixir-mode-hook
;;           (lambda() (add-hook 'before-save-hook 'elixir-format nil t)))

(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

;; ===================================================================
;; Others

;; Set this so that it loads dir-locals.el without prompt
(setq safe-local-variable-directories
      '("~/workspace/inngest"
        "~/workspace/monorepo"
        "~/workspace/inngestgo"
        "~/workspace/infractl"))

;; Projects
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

;; Load Emacs as full screen on startup
(add-hook 'window-setup-hook 'toggle-frame-maximized)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Debugger
(with-eval-after-load 'dape
  ;; Add Golang test configuration
  ;; ref:
  ;; - https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
  ;; - https://github.com/go-delve/delve/blob/master/Documentation/cli/getting_started.md
  ;; - https://github.com/microsoft/vscode-go/blob/master/docs/Debugging-Go-code-using-VS-Code.md
  ;; - https://blog.dornea.nu/2024/11/28/mastering-golang-debugging-in-emacs/
  (add-to-list 'dape-configs
               ;; run test with delve at point
               `(delve
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 fn (dape-config-autoport dape-config-tramp)
                 command "dlv"
                 command-insert-stderr t
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd (lambda() (if (string-suffix-p "_test.go" (buffer-name))
                                           default-directory (dape-cwd)))
                 port :autoport
                 :type "go"
                 :request "launch"
                 :mode (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
                 :program "."
                 :showLog "true"
                 :cwd "."
                 :args (lambda()
                         (require 'which-func)
                         (if (string-suffix-p "_test.go" (buffer-name))
                             (when-let* ((test-name (which-function))
                                         (test-regexp (concat "^" test-name "$")))
                               (if test-name `["-test.run", test-regexp]
                                 (error "No test selected")))
                           [])))))

;; Rust
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))))

(add-to-list '+word-wrap-disabled-modes 'rust-mode)
(add-to-list '+word-wrap-disabled-modes 'rustic-mode)

;; systemd
(add-to-list 'auto-mode-alist '("\\.service\\'" . systemd-mode))

;; Typescript
(setq
 typescript-indent-level 2)

(defun lsp--eslint-before-save (orig-fun)
  "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
  (when lsp-eslint-auto-fix-on-save (lsp-eslint-apply-all-fixes))
  (funcall orig-fun))

;; (add-hook 'typescript-mode-hook
;;           (lambda ()
;;             (advice-add 'lsp--before-save :around #'lsp--eslint-before-save)))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

;; Liquid
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode))

;; beancount (plain text accounting)
;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook #'outline-minor-mode)

;; web-mode
(setq
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2
 web-mode-attr-value-indent-offset 2
 web-mode-comment-formats '(("java" . "/*")
                            ("javascript" . "//")
                            ("typescript" . "//")
                            ("php" . "/*")
                            ("css" . "/*")))

;; Nginx
(add-to-list 'auto-mode-alist '("\\.nginx\\'" . nginx-mode))

;; MDX (Markdown with JSX)
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . gfm-mode))

(defun enable-jsx-in-markdown ()
  (when (and (stringp buffer-file-name)
             (string-match-p "\\.mdx\\'" buffer-file-name))
    (setq-local markdown-enable-wiki-linkification nil)
    (setq-local markdown-enable-math t)
    (markdown-toggle-math t)
    (markdown-toggle-url-hiding nil)
    (web-mode-set-content-type "jsx")
    (web-mode)))

(add-hook 'markdown-mode-hook 'enable-jsx-in-markdown)

;; JSONC mode
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))

;; LLM
;; register Anthropic
(setq
 gptel-backend (gptel-make-anthropic "Claude"
                 :stream t :key gptel-api-key))
