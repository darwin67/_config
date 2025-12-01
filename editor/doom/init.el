;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; (ivy +fuzzy +prescient +icons)
       vertico
       (corfu +dabbrev)

       :ui
       deft
       doom
       doom-dashboard
       doom-quit
       hl-todo
       ;; indent-guides
       ;; neotree
       nav-flash
       (modeline +light)
       minimap
       nav-flash
       ophints
       (popup +defaults)
       unicode
       (vc-gutter +pretty)
       vi-tilde-fringe
       (window-select +numbers)
       ;; workspaces
       zen
       vi-tilde-fringe

       :os
       tty

       :editor
       file-templates
       fold
       (format +onsave +lsp)
       multiple-cursors
       snippets
       word-wrap

       :emacs
       ;;dired
       electric
       ibuffer
       undo
       vc

       :term
       eshell            ; the elisp shell that works everywhere
       shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax
       ;; (spell +enchant)

       :tools
       debugger
       editorconfig
       (eval +overlay)
       lookup
       (lsp +eglot)
       (magit +forge)
       (terraform +lsp)
       tree-sitter
       direnv
       llm

       :lang
       (cc +lsp +tree-sitter)
       data
       (dart +lsp +flutter)
       (elixir +lsp +tree-sitter)
       emacs-lisp
       (erlang +lsp +tree-sitter)
       (go +lsp +tree-sitter)
       (graphql +lsp)
       (javascript +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       (markdown +grip)
       (nix +lsp +tree-sitter)
       (org +brain +dragndrop +gnuplot +hugo +journal +pandoc +roam2)
       (python +cpython +lsp +pyright +tree-sitter)
       (ruby +chruby +rails +lsp +tree-sitter)
       (rust +lsp +tree-sitter)
       (java +lsp +tree-sitter)
       (kotlin +lsp +tree-sitter)
       (sh +tree-sitter)
       (web +tree-sitter)
       (yaml +lsp +tree-sitter)
       (lua +lsp +tree-sitter)
       ;; (zig +lsp +tree-sitter)

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc
       ;;(rss +org)
       ;;twitter

       :config
       ;;literate
       (default +bindings +smartparens))
