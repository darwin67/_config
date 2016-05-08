
;; ===================================================
;;   Email
;; ===================================================

(el-get-bundle wanderlust)
;; (setq user-mail-address "wuddarwin@gmail.com")
(autoload 'wl "wl" "Wanderlust" t)	; autoload ~/.wl on start
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t) ; start wanderlust on another frame
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t) ; autoload drafts
(add-to-list 'auto-mode-alist '("\.wl$" . emacs-lisp-mode))	 ; read ~/.wl as a e-lisp file
