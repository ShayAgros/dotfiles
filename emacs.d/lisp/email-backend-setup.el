;;; Install mu4e and configure the email accounts

;; Transmitting side - built-in Emacs SMTP client

(defun initialize-crediantials ()
  ;; setup the mail address and use name
  (setq user-full-name "Shay Agroskin"
	user-mail-address "shayagr@amazon.com"
	;; smtp crediantials
	message-send-mail-function 'smtpmail-send-it
	smtpmail-default-smtp-server "ballard.amazon.com"
	smtpmail-smtp-server "ballard.amazon.com"
	smtpmail-smtp-service 1587
	smtpmail-local-domain "amazon.com"
	smtpmail-stream-type 'starttls
	smtpmail-smtp-user "shayagr"
	smtpmail-debug-info t))

(require 'smtpmail)
(initialize-crediantials)

;; Receive side - Mu4e

(defun configure-mu4e-basic ()
  ;; email direcotry
  (setq mu4e-maildir (expand-file-name "~/Mail")
	mu4e-drafts-folder "/Drafts"
	mu4e-sent-folder   "/Sent"
	mu4e-trash-folder  "/Trash"
	mu4e-attachment-dir "~/Downloads/mail_attachments"

	mail-user-agent 'mu4e-user-agent

	;; email update configs
	;mu4e-get-mail-command "/home/ANT.AMAZON.COM/shayagr/workspace/scripts/mbsync_all.sh"
	mu4e-get-mail-command "echo query > /tmp/query_email"
	mu4e-update-interval 60
	mu4e-headers-auto-update nil

	;; email dispaly
	mu4e-html2text-command "w3m -T text/html"
	mu4e-compose-signature-auto-include nil

	mu4e-show-images t
	mu4e-compose-dont-reply-to-self t
	mu4e-view-show-addresses 't

	;; This would avoid creating duplicate UID
	mu4e-change-filenames-when-moving t

	;; Don't print updates regarding what you're doing
	mu4e-index-update-in-background t
	mu4e-hide-index-messages t

	mu4e-compose-format-flowed t

	;; use gnus format when displaying emails (would make images show up)
	mu4e-view-use-gnus t

	;; Modify mu4e contacts
	mu4e-contact-process-function 'my-mu4e-contact-processor

	;; enable inline images
	mu4e-view-show-images t))

(defun configure-mu4e-bookmarks ()
  "Saved searches"
  (setq mu4e-bookmarks '())

  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "netdev to/cc Amzn"
		 :query "list:netdev.vger.kernel.org and (cc:amazon or to:amazon)"
		 :key   ?a))

  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "Linux BPF, unread"
		 :query "list:bpf.vger.kernel.org AND flag:U"
		 :key   ?b
		 ))

  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "work unread messages"
		 :query "flag:U and maildir:/amazon/*"
		 :key   ?w))


  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "netdev unread"
		 :query "list:netdev.vger.kernel.org AND flag:U"
		 :key   ?l))
  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "awesome ml"
		 :query "list:awesome.awesomeWM.github.com"
		 :key   ?o))

  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "kernel newbies"
		 :query "list:kernelnewbies.kernelnewbies.org"
		 :key   ?n))


  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "gmail unread messages"
		 :query "maildir:/gmail/ flag:u not flag:list"
		 :key ?u)))


(defun configure-mu4e-actions ()
  "Configure the actions that can be done on each separate email"
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("apply to net-next" . mu4e-apply-to-net-next) t)
  (add-to-list 'mu4e-view-actions '("Open in a new buffer" . ed/preview-some-mail-at) t))


(defun configure-mu4e-shortcuts ()
  "Configure maildirs. This is pretty much the same as actions but
with fewer search terms"
  (setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"        . ?i)
         ("/gmail/linux"        . ?l)
         ("/gmail/LinuxNewbies" . ?d)
         ("/amazon/INBOX"       . ?a))))


(defun my-mu4e-contact-processor (contact)
  (cond
   ;; Remove prod.sim emails
   ;; e.g. Arthur Kiyanovski <issues+akiyano@prod.sim.a2z.com>
   ;; Arthur Kiyanovski <issues+akiyano@prod.sim.a2z.com>
   ;; (if (string-match-p "prod.sim.a2z.com" "Arthur Kiyanovski <issues+akiyano@prod.sim.a2z.com>") t nil)
   ((string-match-p "prod.sim.a2z.com" contact)
    nil)
   ((string-match-p "sameeh@daynix.com" contact)
    nil)
   ((string-match-p "sameehj@amazon.com" contact)
    "\"Jubran Samih\" <sameehj@amazon.com>")
      ;; jonh smiht --> John Smith
   ((string-match-p "agrosshay@gmail.com" contact)
    "Shay Agroskin <agrosshay@gmail.com>")

   (t contact)))

;; This was installed from source, not the package manager
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(configure-mu4e-basic)
(configure-mu4e-bookmarks)
(configure-mu4e-actions)
(configure-mu4e-shortcuts)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))


;; Setup custom key-bindings
(add-hook 'mu4e-headers-mode-hook
	  (lambda()
	    (local-set-key (kbd "J") (lambda ()
				       (interactive)
				       (mu4e-headers-mark-thread nil '(read))
				       (mu4e-mark-execute-all t)))
	    (local-set-key (kbd "N") 'mu4e-goto-next-thread)
	    (local-set-key (kbd "P") 'mu4e-goto-previous-thread)
	    ))

(provide 'email-backend-setup)
