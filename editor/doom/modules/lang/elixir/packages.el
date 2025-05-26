;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el

;; https://github.com/wkirschbaum/elixir-ts-mode
(package! elixir-ts-mode :pin "e518e9a086d3d3c427411fbe3aa22f34c2e85614")

;; https://github.com/ananthakumaran/exunit.el
(package! exunit :pin "8de56e3fd50832e5f0435bba8eb13c7292cb1ee1")

;; https://github.com/elixir-editors/emacs-elixir
(package! elixir-format
  :recipe (:host github :repo "elixir-editors/emacs-elixir" :files ("elixir-format.el"))
  :pin "00d6580a040a750e019218f9392cf9a4c2dac23a")

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
