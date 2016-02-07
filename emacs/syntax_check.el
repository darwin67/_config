
;; ===================================================
;;   Syntax checking
;; ===================================================

;; Fly-check
;;  need to install texinfo in OS X
;;  - brew install texinfo
;;  - brew link texinfo -f
(el-get-bundle flycheck
  (global-flycheck-mode)
)
;; (el-get-bundle flycheck-color-mode-line)
;; (eval-after-load "flycheck"
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; )


;; C++
(el-get-bundle flycheck-google-cpplint)
(defun cpp-check ()
  (interactive)
  (require 'flycheck-google-cpplint)
  ;; Add Google C++ Style checker.
  ;; In default, syntax checked by Clang and Cppcheck.
  (flycheck-add-next-checker 'c/c++-cppcheck
			     '(warnings-only . c/c++-googlelint))
)
(add-hook 'c++-mode-hook 'cpp-check)
