;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el

;; https://github.com/wkirschbaum/elixir-ts-mode
(package! elixir-ts-mode :pin "b08d399e29714fe3d968db925aec0e1dfac54d89")

;; https://github.com/ananthakumaran/exunit.el
(package! exunit :pin "ee06b14b61beaf59d847802b83b027ac991f3496")

;; https://github.com/elixir-editors/emacs-elixir
(package! elixir-format
  :recipe (:host github :repo "elixir-editors/emacs-elixir" :files ("elixir-format.el"))
  :pin "00d6580a040a750e019218f9392cf9a4c2dac23a")

(when (modulep! :checkers syntax -flymake)
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
