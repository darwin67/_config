;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; (ivy +fuzzy +prescient +icons) ; a search engine for love and life
       vertico
       (corfu +dabbrev)

       :ui
       deft
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW/BUG/XXX
       ;; indent-guides
       ;; neotree
       nav-flash
       (modeline +light)            ; snazzy, Atom-inspired modeline, plus API
       minimap
       nav-flash           ; blink the current line after jumping
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       unicode             ; extended unicode support for various languages
       (vc-gutter +pretty)           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       (window-select +numbers)       ; visually switch windows
       ;; workspaces          ; tab emulation, persistence & separate workspaces
       zen
       vi-tilde-fringe  ; Displays a tilde(~) in the left fringe to indicate an empty line, similar to Vi.

       :os
       tty

       :editor
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format +onsave +lsp)    ; automated prettiness
       multiple-cursors    ; editing in many places at once
       snippets            ; my elves. They type so I don't have to
       word-wrap           ; soft wrapping with language-aware indent

       :emacs
       ;;dired             ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       ibuffer             ; interactive buffer management
       undo                ; persistent, smarter undo for your inevitable mistakes
       vc                  ; version-control and Emacs, sitting in a tree

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
       (cc +lsp +tree-sitter)           ; C/C++/Obj-C madness
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
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
