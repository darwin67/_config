;; mode:-*-emacs-lisp-*-
;; ===================================================
;;   Wanderlust
;; ===================================================

(setq wl-insert-message-id nil) ; let the SMTP servers handle the message-id and stop warning from wanderlust

;; for non ascii-characters in folder-names
(setq elmo-imap4-use-modified-utf7 t)

;; Folder pane
(setq wl-stay-folder-window t)		; show the folder pane on the left
(setq wl-folder-window-width 40)	; set the width of the folder pane

(setq wl-folder-desktop-name "Emails")	; Rename Desktop name

(setq wl-message-ignored-field-list '("^.*:")) ; hide as many fields in the email header as possible

;; Fields in the e-mail header that I want to see even
;; if they match the regex in wl-message-ignored-field-list
(setq wl-message-visible-field-list
      '("^\\(To\\|Cc\\|Bcc\\):"
	"^\\(From\\|Reply-To\\):"
	"^\\(Posted\\|Date\\):"
	"^Subject:"
	"^Organization:"
	"^Message-Id:"))

;; Sort the order of the email head
(setq wl-message-sort-field-list
      '("^From:"
	"^Organization:"
	"^Subject:"
	"^Date:"
	"^To"
	"^Cc"
	"^Bcc"))

;; Folders to be check periodically
(setq wl-biff-check-folder-list '("%inbox"))

;; My email address lists
(setq wl-user-mail-address-list '("wuddarwin@gmail.com" "darwinwu67@gmail.com"))

;; Posting templates for multiple email addresses (receiving settings are done in ~/.folders)
(setq wl-template-alist
      '(
	("wuddarwin@gmail.com"		; Gmail
	 (wl-from . "Darwin D. Wu <wuddarwin@gmail.com>")
	 ("From" . wl-from)
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-smtp-authenticate-type . "plain")
	 (wl-smtp-posting-user . "wuddarwin@gmail.com")
	 (wl-smtp-posting-server . "smtp.gmail.com")
	 (wl-local-domain . "gmail.com")
	 (wl-fcc-force-as-read t)
	 (wl-default-spec "%")
	 (signature-file-name . "~/Mail/.signature"))
	("darwinwu67@gmail.com"		; Gmail backup
	 (wl-from . "Darwin D. Wu <darwinwu67@gmail.com>")
	 ("From" . wl-from)
	 (wl-smtp-connection-type . 'starttls)
	 (wl-smtp-posting-port . 587)
	 (wl-smtp-authenticate-type . "plain")
	 (wl-smtp-posting-user . "darwinwu67@gmail.com")
	 (wl-smtp-posting-server . "smtp.gmail.com")
	 (wl-local-domain . "gmail.com")
	 (wl-fcc-force-as-read t)
	 (signature-file-name . "~/Mail/.signature_sub"))
	;; ("dwu@cerego.com"
	;;  (wl-from . "Darwin D. Wu <dwu@cerego.com>")
	;;  ("From" . wl-from)
	;;  (wl-smtp-connection-type . 'ssl)
	;;  (wl-smtp-posting-port . 587)
	;;  (wl-smtp-authenticate-type . "login")
	;;  (wl-smtp-posting-user . "dwu@cerego.com")
	;;  (wl-smtp-posting-server . "smtp.gmail.com")
	;;  (wl-local-domain . "gmail.com")
	;;  (wl-fcc-force-as-read t)
	;;  ("Organization" . "Cerego LLC")
	;;  (signature-file-name . "~/.signature_work"))
	))

(setq wl-forward-subject-prefix "Fwd: " )    ;; use "Fwd: " not "Forward: "

;; Automatically select the correct template based on which folder I'm visiting
(setq wl-draft-config-matchone t) ;; If non-nil, applied only one element of `wl-draft-config-alist'.
(setq wl-draft-config-alist
      '(
	(;; If I start a draft from my work e-mail folder and I'm using my
	 ;; personal computer (from home) use the template "Work-From-Home". I
	 ;; use a two different templates for my work E-Mail because I don't
	 ;; have access to the smtp server of my work when I'm at home. But
	 ;; since I can ssh to it i redirect a port to be able to sent e-mail
	 ;; from home though the smtp server of my work
	 (and (string-match ".*work" wl-draft-parent-folder) (string-match "laptop" system-name))
	 (template . "Work-From-Home")
	 )
	(;; If I start a draft from my work e-mail folder and I'm using my
	 ;; work computer, use the "Work" template
	 (and (string-match ".*work" wl-draft-parent-folder) (string-match "work-computer" system-name))
	 (template . "Work"))
	(;; If I start a draft from any other folder, use the "gmail" template.
	 (not (string-match ".*work" wl-draft-parent-folder))
	 (template . "gmail"))
	))

;; Apply wl-draft-config-alist as soon as you enter in a draft buffer. Without
;; this wanderlust would apply it only when actually sending the e-mail.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

;; Enables auto-fill-mode in the draft buffer
(add-hook 'wl-mail-setup-hook 'auto-fill-mode)

;; Setting as "t" means that wanderlust should use a new frame for the draft
;; (setq wl-draft-use-frame t)

;; Proportion of the summary and message windows
;; (setq wl-message-window-size '(3 . 7))

;; If you want to manually change the template use C-c C-j in a draft buffer
;; (wl-template-select). The four lines below allow changint the template with
;; the arrow keys
(define-key wl-template-mode-map (kbd "<right>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<left>") 'wl-template-prev)
(define-key wl-template-mode-map (kbd "<up>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<down>") 'wl-template-prev)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; Use orgstruct++-mode in the draft buffer
;; (add-hook 'mail-mode-hook 'turn-on-orgstruct)
(add-hook 'mail-mode-hook 'turn-on-orgstruct++)


;; Set the key "f" to browse-url when I'm reading an E-mail. If instead of an url I have an HTML code I can simple select the code and hit "F"
(add-hook 'mime-view-mode-hook
	  (lambda ()
	    (local-set-key "f" 'browse-url)
	    (local-set-key "F" 'browse-url-of-region)))

;; Auto-refile With this wanderlust suggests folders to refile a message when I
;; press the "o" in the summary. Then I can simple press ENTER to accept or
;; write (with TAB completion) the folder I want to refile the message For
;; instance, if I want wanderlus to suggest the gmail trash folder when I refile
;; an E-mail from launchpad about a bug or an E-mail from the org-mode list I
;; may use
(setq wl-refile-rule-alist
      '(
	("To" ("emacs-orgmode@gnu.org" . "%[Gmail]/Lixeira:\"wuddarwin@gmail.com\"/clear@imap.gmail.com:993!"))
	("Subject" ("\\[Bug [0-9]*\\]" . "%[Gmail]/Lixeira:\"wuddarwin@gmail.com\"/clear@imap.gmail.com:993!")) ; E-Mails do Launchpad
	)) ; Notice that the folders must match what I have used in the .folders file

;; xxxxxxxxxxxxxxx Integration with bbdb xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; (require 'bbdb-wl)
;; (bbdb-wl-setup)

;; (setq bbdb-use-pop-up nil ;; disable pop-ups
;;       ;; bbdb-pop-up-target-lines 5   ; only useful if pop-ups are enabled
;;       )

;; ;; Set bbdb-user-mail-names with a regex that matches both of my e-mail accounts
;; ;; Pra que serve bbdb-user-mail-names?
;; (setq bbdb-user-mail-names (regexp-opt '("USERNAME@gmail.com" "USERNAME@work.something")))

;; ;; auto collection
;; (setq bbdb/mail-auto-create-p 'prompt) ;; bbdb asks me if I want to save an E-mail

;; ;; exceptional folders against auto collection. The regex must match all folders that we do not want auto-colection. This is what I use
;; (setq bbdb-wl-ignore-folder-regexp "^@\\|Org-Mode\\|Muse\\|Doxygen\\|Launchpad")


;; (setq bbdb-north-american-phone-numbers-p nil)


;; ;; shows the name of bbdb in the summary. Default value of this variable is
;; ;; wl-summary-default-from which does not use bbdb, but the wanderlust address
;; ;; book instead
;; (setq wl-summary-from-function 'bbdb-wl-from-func)


;; ;; Using BBDB for pet names in wanderlust
;; (setq wl-summary-get-petname-function 'bbdb-wl-get-petname)


;; You can complete address with BBDB by `M-TAB' in draft buffer.
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


;; xxxxxxxxxxxxxxx Spam check xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;; (setq elmo-spam-scheme 'bogofilter) ;; sa for spamassassin, see the elmo-spam-scheme
;; ;; docs for alternatives
;; (require 'wl-spam)
;; (setq wl-spam-folder "spam folder as defined in the .folders file")

;; ;; Automatically check for spam in the folders that match the regex
;; (setq wl-spam-auto-check-folder-regexp-list '("regex here"))


;; ;; Folders that should not be checked for spam. Since gmail already has spam checking I make the regex match it
;; (setq wl-spam-ignored-folder-regexp-list
;;       (list (regexp-opt (list wl-draft-folder
;; 			      wl-trash-folder
;; 			      wl-queue-folder
;; 			      "my_gmail_username"
;; 			      "Gmail"
;; 			      "gmail"))))
;; xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
