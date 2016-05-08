
;; ===================================================
;;   Email
;; ===================================================

(el-get-bundle wanderlust)
;; Without this Emacs thinks my E-Mail is something like <myname>@ubuntu-asus
(setq user-mail-address "wuddarwin@gmail.com")

;; autoload ~/.wl on start
(autoload 'wl "wl" "Wanderlust" t)

;; start wanderlust on another frame
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)

;; autoload drafts
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; read ~/.wl as a e-lisp file
(add-to-list 'auto-mode-alist '("\.wl$" . emacs-lisp-mode))
