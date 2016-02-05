
;; ===================================================
;;   Navigation
;; ===================================================

;; Wind move (http://www.emacswiki.org/emacs/WindMove)
;;   Using Ctr-c <Arrow> to move between buffers instead of typing Ctr-x o
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)
)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Move, switch buffer around
(el-get-bundle buffer-move)
(global-set-key (kbd "C-c k") 'buf-move-up)
(global-set-key (kbd "C-c j") 'buf-move-down)
(global-set-key (kbd "C-c h") 'buf-move-left)
(global-set-key (kbd "C-c l") 'buf-move-right)

;; Tab management
(defun previous-frame ()
  "A function to go to the previous frame."
  (interactive)
  (other-frame -1)
)
(global-set-key (kbd "C-x t n") 'make-frame-command)
(global-set-key (kbd "C-x n t") 'other-frame)
;; (global-set-key (kbd "C-x p t") 'previous-frame)

;; Move text up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	(exchange-point-and-mark))
    (let ((column (current-column))
	  (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
	(forward-line)
	(when (or (< arg 0) (not (eobp)))
	  (transpose-lines arg)
	  (when (and (eval-when-compile
		       '(and (>= emacs-major-version 24)
			     (>= emacs-minor-version 3)))
		     (< arg 0))
	    (forward-line -1)))
	(forward-line -1))
      (move-to-column column t)))
  )
)

(defun move-text-down (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines down."
    (interactive "*p")
    (move-text-internal arg)
)

(defun move-text-up (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines up."
    (interactive "*p")
    (move-text-internal (- arg))
)

;; use C-x z (N) to repeat command
(global-set-key (kbd "C-x <up>") 'move-text-up)
(global-set-key (kbd "C-x <down>") 'move-text-down)
