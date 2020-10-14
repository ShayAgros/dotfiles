;;; init-email.el --- mail client
;;; Commentary:
;;; This package includes all configurations to set mu4e + smtp
;;; with Gmail and Amazon email
;;; Code:

;; Custom functions
(defun mu4e-apply-to-net-next (msg)
  "Apply `MSG' as a git patch to net-next directory in ~/workspace/net-next"
    (let ((default-directory "~/workspace/net-next/"))
    (shell-command
     (format "git am %s"
             (shell-quote-argument (mu4e-message-field msg :path))))))

;; email configuration functions

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

(defun configure-mu4e-basic ()
  ;; email direcotry
  (setq mu4e-maildir (expand-file-name "~/Mail")
	mu4e-drafts-folder "/Drafts"
	mu4e-sent-folder   "/Sent"
	mu4e-trash-folder  "/Trash"
	mu4e-attachment-dir "~/Downloads/mail_attachments"

	mail-user-agent 'mu4e-user-agent
	
	;; email update configs
	mu4e-get-mail-command "/home/ANT.AMAZON.COM/shayagr/workspace/scripts/mbsync_all.sh"
	mu4e-update-interval 180
	mu4e-headers-auto-update t

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
	mu4e-view-use-gnus t))

(defun configure-mu4e-bookmarks ()
  ;; Empty the initial bookmark list
  (setq mu4e-bookmarks '())

  (add-to-list 'mu4e-bookmarks
	       '(
		 :name  "netdev to/cc Amzn"
		 :query "list:netdev.vger.kernel.org and (cc:amazon or to:amazon)"
		 :key   ?a))

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
		 :name  "unread"
		 :query "flag:u"
		 :key ?u)))

(defun configure-mu4e-actions ()
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("apply to net-next" . mu4e-apply-to-net-next) t))

(defun my-mu4e-contact-processor (contact)
  (cond
   ;; jonh smiht --> John Smith
   ((string-match "agrosshay@gmail.com" contact)
    "Shay Agroskin <agrosshay@gmail.com>")
   ;; Remove prod.sim emails
   ((string-match "prod.sim.a2z.com" contact)
    nil)
   ;; Arthur Kiyanovski <issues+akiyano@prod.sim.a2z.com>
   (t contact)))

; Vanilla configs

;; Sending side: emacs built-it smtp client
(require 'smtpmail)
(initialize-crediantials)

;; Receiving side. Mu4e

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(configure-mu4e-basic)

(setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"        . ?i)
         ("/gmail/linux"        . ?l)
         ("/gmail/LinuxNewbies" . ?d)
         ("/amazon/INBOX"       . ?a)))

(configure-mu4e-bookmarks)
(configure-mu4e-actions)

;; Modify mu4e contacts
(setq mu4e-contact-process-function 'my-mu4e-contact-processor)

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))


;; colorize patch-based emails

; mu4e hacks

;; colorize patch emailsn
(require 'mu4e-patch)
(add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight)

;; Add support for writing emails in org-mode

(require 'org-mu4e)
(use-package org-mime
  :ensure t
  :config
  (setq org-mu4e-convert-to-html t))

(defun org-mime-org-buffer-htmlize ()
  "Create an email buffer containing the current org-mode file
  exported to html and encoded in both html and in org formats as
  mime alternatives."
  (interactive)
  (org-mime-send-buffer 'html)
  (message-goto-to))

(add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

;; Add Org mode capture command
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

(defun bms/toggle-mu4e-gnus ()
  (interactive)
  "Toggle between old mu4e view mode and gnus-view-mode."
  (if (equal mu4e-view-use-gnus t)
      (setq mu4e-view-use-gnus nil)
    (setq mu4e-view-use-gnus t))
  (mu4e-view-refresh))

;; Set key-bindings

(add-hook 'mu4e-headers-mode-hook
	  (lambda()
	    (local-set-key (kbd "J") (lambda ()
				       (interactive)
				       (mu4e-headers-mark-thread nil '(read))))
	    (local-set-key (kbd "J") (lambda ()
				       (interactive)
				       (mu4e-headers-mark-thread nil '(read))))))

(provide 'init-email)
;;; init-email.el ends here
