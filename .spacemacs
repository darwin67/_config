;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs          ;; allowed values ('spacemacs, 'spacemacs-base)
   dotspacemacs-enable-lazy-installation 'unused ;; allowed values ('all 'unused nil)
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(helm
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-idle-delay 0.5
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     cmake
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     (org :variables
          org-enable-github-support t
          org-enable-org-journal-support t
          org-enable-hugo-support t
          org-enable-trello-support t
          org-enable-epub-support t)
     ;; semantic
     syntax-checking
     docker
     nginx
     systemd
     (lsp :variables
          lsp-navigation 'simple
          lsp-prefer-flymake nil
          lsp-ui-doc-enable t
          lsp-ui-doc-include-signature nil
          lsp-ui-sideline-enable t
          lsp-ui-sideline-show-symbol nil
          lsp-ui-sideline-ignore-duplicate t
          )
     ;; dap
     yaml
     (terraform :variables terraform-auto-format-on-save t)
     node
     xclipboard
     salt

     ;; version control
     git
     github
     (version-control :variables
                      version-control-diff-tool 'git-gutter)

     ;; Framework
     react
     ruby-on-rails
     phoenix

     ;; Languages
     asm
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t
            c-c++-enable-google-style t
            c-c++-enable-google-newline t
            c-c++-enable-clang-format-on-save t
            c-c++-backend 'clang)
     ;; (dart :variables
     ;;       dart-enable-analysis-server t
     ;;       dart-format-on-save t)
     (elixir :variables
             elixir-backend 'lsp
             elixir-ls-path "~/.lsp/elixir-ls")
     erlang
     emacs-lisp
     (elm :variables
          elm-format-on-save t
          elm-sort-imports-on-save t)
     (go :variables
         godoc-at-point-function 'godoc-gogetdoc
         go-backend 'lsp
         go-use-gometalinter t
         go-format-before-save t)
     html
     (javascript :variables
                 javascript-backend 'lsp
                 javascript-fmt-tool 'prettier)
     ;; kotlin
     markdown
     protobuf
     (python :variables
             python-backend 'lsp
             python-test-runner 'pytest
             python-sort-imports-on-save t)
     (ruby :variables
           ruby-version-manager 'rbenv
           ruby-test-runner 'rspec
           ruby-highlight-debugger-keywords t)
     (rust :variables
           rust-format-on-save t
           rust-backend 'lsp)
     shell-scripts
     sql
     (typescript :variables
                 typescript-fmt-on-save t
                 typescript-fmt-tool 'typescript-formatter
                 typescript-backend 'lsp)
     vimscript
     )
   dotspacemacs-additional-packages '(dictionary
                                      buffer-move
                                      editorconfig)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only)) ;; allowed values ('used-only 'used-but-keep-unused 'all)

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'emacs ;; allowed values ('emacs 'vim 'hybrid)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official ;; allowed values ('official 'random nil)
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t ;; True if the home buffer should respond to resize events.
   dotspacemacs-scratch-mode 'text-mode
   ;; Press <SPC> T n to cycle to the next theme in the list
   dotspacemacs-themes '(zenburn
                         molokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Meslo LG S for Powerline"
                               :size 11.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used there.
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon start.
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache ;; allowed values ('original 'cache nil)
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always ;; allowed values ('always 'source)
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami ;; allowed values ('evil 'origami)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all ;; allowed values ('any 'current 'all nil)
   dotspacemacs-persistent-server nil
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'all ;; allowed values ('all 'trailing 'changed nil)
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; ==================================================
  ;;   Variables

  (setq
   configuration-layer-elpa-archives
   '(("melpa" . "melpa.org/packages/")
     ("org" . "orgmode.org/elpa/"))

   ;; Stop emacs adding the utf-8 magic comment
   ruby-insert-encoding-magic-comment nil

   ;; Javascript indentaion
   js2-basic-offset 2
   js-indent-level 2
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2

   ;; Elixir
   flycheck-elixir-credo-strict t

   ;; Disable flycheck for certain modes
   flycheck-disabled-checkers '(chef-foodcritic elm)

   dumb-jump-selector 'helm
   flycheck-pos-tip-timeout 10

   ;; LSP
   lsp-sideline-delay 0.5

   ;; Other
   ;; x86-lookup-pdf "~/_config/etc/architecture-instruction-set-extensions-programming-reference.pdf"

   ;; Do not try to grab everything
   forge-pull-notifications nil
   )

  ;; ==================================================
  ;;   Modes

  ;; Enable company mode globally
  (global-company-mode)

  ;; Use emacs to edit commits
  (global-git-commit-mode t)

  ;; Use editorconfig
  (editorconfig-mode t)

  ;; language support for C++
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

  ;; register berksfile as ruby files
  (add-hook 'ruby-mode-hook 'lsp)
  (add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))

  ;; Python
  (add-hook 'python-mode-hook 'lsp)

  ;; Golang
  (add-hook 'go-mode-hook 'lsp)

  ;; treat .tsx as typescript files
  (add-hook 'typescript-mode-hook 'lsp)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

  ;; treat .nginx as nginx files
  (add-to-list 'auto-mode-alist '("\\.nginx\\'" . nginx-mode))

  ;; Elixir
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

  ;; ==================================================
  ;;   Key bindings
  (global-set-key (kbd "C-x -") 'split-window-below)
  (global-set-key (kbd "C-x |") 'split-window-right)

  (global-set-key (kbd "C-c ]") 'dumb-jump-go)
  (global-set-key (kbd "C-c C-]") 'spacemacs/jump-to-definition)
  (global-set-key (kbd "C-c }") 'spacemacs/jump-to-definition-other-window)
  (global-set-key (kbd "C-c [") 'dumb-jump-back)
  (global-set-key (kbd "C-c \\") 'dumb-jump-quick-look)

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
    (next-line 1)
    (when newline-and-indent (indent-according-to-mode)))
  (defun open-previous-line (arg)
    "Open a new line before the current one."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (when newline-and-indent (indent-according-to-mode)))

  (global-set-key (kbd "C-o") 'open-next-line)
  (global-set-key (kbd "M-o") 'open-previous-line)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-color-mode-line terraform-mode hcl-mode flycheck-elm elm-mode org-mime rjsx-mode yapfify yaml-mode x86-lookup ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tide typescript-mode tagedit systemd stickyfunc-enhance srefactor sql-indent spaceline powerline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails rake inflections popwin pip-requirements persp-mode pcre2el paradox spinner ox-gfm origami orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file nginx-mode neotree nasm-mode mwim move-text mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode js2-refactor multiple-cursors js2-mode js-doc insert-shebang info+ indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md fuzzy flycheck-rust seq flycheck-pos-tip flycheck pkg-info epl flx-ido flx fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight emmet-mode elisp-slime-nav editorconfig dumb-jump dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat disaster diminish diff-hl dictionary link connection define-word dactyl-mode cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-quickhelp pos-tip company-go go-mode company-c-headers company-anaconda company column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby cargo rust-mode bundler inf-ruby buffer-move bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#313131"))))
 '(company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (import-js grizzl add-node-modules-path flycheck-color-mode-line terraform-mode hcl-mode flycheck-elm elm-mode org-mime rjsx-mode yapfify yaml-mode x86-lookup ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toml-mode toc-org tide typescript-mode tagedit systemd stickyfunc-enhance srefactor sql-indent spaceline powerline smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode projectile-rails rake inflections popwin pip-requirements persp-mode pcre2el paradox spinner ox-gfm origami orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file nginx-mode neotree nasm-mode mwim move-text mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint less-css-mode js2-refactor multiple-cursors js2-mode js-doc insert-shebang info+ indent-guide ibuffer-projectile hydra hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md fuzzy flycheck-rust seq flycheck-pos-tip flycheck pkg-info epl flx-ido flx fish-mode fill-column-indicator feature-mode fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit ghub let-alist with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight emmet-mode elisp-slime-nav editorconfig dumb-jump dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat disaster diminish diff-hl dictionary link connection define-word dactyl-mode cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-shell company-quickhelp pos-tip company-go go-mode company-c-headers company-anaconda company column-enforce-mode coffee-mode cmake-mode clean-aindent-mode clang-format chruby cargo rust-mode bundler inf-ruby buffer-move bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#313131")))))
)
