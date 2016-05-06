
;; ===================================================
;;   Email
;; ===================================================

(require 'gnus)
(el-get-bundle 'emacs-w3m)

;; Basic settings
(setq gnus-use-full-window nil)
(setq nnml-directory "~/gmail")
(setq message-directory "~/gmail")
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

(setq gnus-mime-display-multipart-related-as-mixed nil)
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)
(setq w3m-default-desplay-inline-images t)

(add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications) ; get notifications for new emails

;; (eval-after-load 'gnus-topic
;;   '(progn
;;      (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
;;      (setq gnus-topic-topology '(("Gnus" visible)
;; 				 (("misc" visible))
;; 				 (("gmail" visible nil nil))))

;;      (setq gnus-topic-alist '(("gmail" ; the key of topic
;; 			       "INBOX"
;; 			       "[Gmail]/Sent Mail"
;; 			       "Drafts")
;; 			      ("misc" ; the key of topic
;; 			       "nnfolder+archive:sent.2015-12"
;; 			       "nnfolder+archive:sent.2016"
;; 			       "nndraft:drafts")
;; 			      ("Gnus")))))
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)		 ; tree view of mail folders

;; Getting email
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

;; Sending email
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "wuddarwin@gmail" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)
