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
       (vertico +icons)
       (corfu +icons)

       :ui
       deft
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW/BUG/XXX
       ;; (emoji +ascii +github +unicode)
       neotree
       (modeline +light)            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink the current line after jumping
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       unicode             ; extended unicode support for various languages
       (vc-gutter +pretty)           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       (window-select +numbers)       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       zen

       :os
       tty

       :editor
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format +onsave)    ; automated prettiness
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
       syntax              ; tasing you for every semicolon you forget
       ;; (spell +enchant)

       :tools
       debugger
       editorconfig        ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       (lsp +eglot)
       (magit +forge)      ; a git porcelain for Emacs
       (terraform +lsp)
       tree-sitter
       direnv
       llm                 ; large language models

       :lang
       (cc +lsp +tree-sitter)           ; C/C++/Obj-C madness
       data                ; config/data formats
       (dart +lsp +flutter)     ; paint ui and not much else
       (elixir +lsp +tree-sitter)  ; erlang done right
       emacs-lisp          ; drown in parentheses
       (erlang +lsp +tree-sitter)  ; an elegant language for a more civilized age
       (go +lsp +tree-sitter)      ; the hipster dialect
       (graphql +lsp)
       (javascript +lsp +tree-sitter) ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp +tree-sitter)
       (markdown +grip)            ; writing docs for people to ignore
       (nix +lsp +tree-sitter)
       (org +dragndrop +gnuplot +journal +pandoc +pretty +roam2)                 ; organize your plain life in plain text
       (python +cpython +lsp +pyright +tree-sitter) ; beautiful is better than ugly
       (ruby +chruby +rails +lsp +tree-sitter)  ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter)    ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (java +lsp +tree-sitter)
       (kotlin +lsp +tree-sitter)
       (sh +tree-sitter)           ; she sells {ba,z,fi}sh shells on the C xor
       (web +tree-sitter)          ; the tubes
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
