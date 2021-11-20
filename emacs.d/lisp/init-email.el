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
     (format "git am -3 %s"
             (shell-quote-argument (mu4e-message-field msg :path))))))

(defun mu4e-headers-mark-thread-using-markpair (markpair &optional subthread)
  "Mark the thread at point using the given markpair. If SUBTHREAD is
non-nil, marking is limited to the message at point and its
descendants."
  (let* ((mark (car markpair))
         (allowed-marks (mapcar 'car mu4e-marks)))
    (unless (memq mark allowed-marks)
      (mu4e-error "The mark (%s) has to be one of: %s"
                  mark allowed-marks)))
  ;; note: the thread id is shared by all messages in a thread
  (let* ((msg (mu4e-message-at-point))
         (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
         (path      (mu4e~headers-get-thread-info msg 'path))
         (last-marked-point))
    (mu4e-headers-for-each
     (lambda (mymsg)
       (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id)))
         (if subthread
             ;; subthread matching; mymsg's thread path should have path as its
             ;; prefix
             (when (string-match (concat "^" path)
                                 (mu4e~headers-get-thread-info mymsg 'path))
               (mu4e-mark-at-point (car markpair) (cdr markpair))
               (setq last-marked-point (point)))
           ;; nope; not looking for the subthread; looking for the whole thread
           (when (string= thread-id my-thread-id)
             (mu4e-mark-at-point  (car markpair) (cdr markpair))
             (setq last-marked-point (point)))))))
    (when last-marked-point
      (goto-char last-marked-point)
      (mu4e-headers-next))))

(defun mu4e-goto-next-thread ()
  "Jump the the next thread"
  (interactive)
    (let* ((msg (mu4e-message-at-point))
	   (thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
      
      (mu4e-headers-find-if
       (lambda (mymsg)
	 (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id)))
	   (not (string= thread-id my-thread-id))
	   )
	 )
       )
      )
    )

(defun mu4e-goto-previous-thread ()
  "Jump the the previous thread or the head of the current thread"
  (interactive)
    (let* ((msg (mu4e-message-at-point))
	   (thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
      
      (mu4e-headers-find-if-next
       (lambda (mymsg)
	 (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
	       (my-thread-path (mu4e~headers-get-thread-info mymsg 'path)))
	   (or
	    (string= my-thread-id my-thread-path)
	    (and (not (string= thread-id my-thread-id))
		 (string= my-thread-id my-thread-path)))
	   )
	 )
       ;; search backward
       t)
      )
    )

(defun mu4e-apply-thread-to-net-next ()
  "Apply a patchset on net-next"
  (interactive)
  (let* ((msg mu4e~view-message)
	 (default-directory "~/workspace/net-next/")
	 (msgid (mu4e-message-field msg :message-id))
	 (date (format-time-string mu4e-view-date-format (mu4e-message-field msg :date))))
    (setq last_commit (shell-command-to-string
		       (format "git -C %s log -1 --before '%s' --pretty='%s'" default-directory date "%h")
		       ))
    
    (message last_commit)))

(setq last_commit (shell-command-to-string
		   (format "git -C ~/ena-drivers log -1 --pretty='%s'" "%h")
		   ))
(message last_commit)

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
		 :name  "unread"
		 :query "flag:u"
		 :key ?u)))

(defun configure-mu4e-actions ()
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("apply to net-next" . mu4e-apply-to-net-next) t))

(defun my-mu4e-contact-processor (contact)
  (cond
   ;; Remove prod.sim emails
   ;; e.g. Arthur Kiyanovski <issues+akiyano@prod.sim.a2z.com>
   ((string-match "prod.sim.a2z.com" contact)
    nil)
   ((string-match "sameeh@daynix.com" contact)
    nil)
   ((string-match "sameehj@amazon.com" contact)
    "\"Jubran Samih\" <sameehj@amazon.com>")
      ;; jonh smiht --> John Smith
   ((string-match "agrosshay@gmail.com" contact)
    "Shay Agroskin <agrosshay@gmail.com>")

   (t contact)))

; Vanilla configs

;; Sending side: emacs built-it smtp client
(require 'smtpmail)
(initialize-crediantials)

;; Receiving side. Mu4e

(add-to-list 'load-path "/home/ANT.AMAZON.COM/shayagr/workspace/Software/mu/build/mu4e")
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

;; colorize patch
(require 'mu4e-patch)
(add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight)

;; Add support for writing emails in org-mode

;; (require 'org-mu4e)
;; (use-package org-mime
;;   :config
;;   (setq org-mu4e-convert-to-html t))

;; (defun org-mime-org-buffer-htmlize ()
;;   "Create an email buffer containing the current org-mode file
;;   exported to html and encoded in both html and in org formats as
;;   mime alternatives."
;;   (interactive)
;;   (org-mime-send-buffer 'html)
;;   (message-goto-to))

;; (add-hook 'org-ctrl-c-ctrl-c-hook 'htmlize-and-send t)

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

;; Choose contacts using ivy-mode
;; (require 'mu4e-ivy-contacts)

;; Set key-bindings

(add-hook 'mu4e-headers-mode-hook
	  (lambda()
	    (local-set-key (kbd "J") (lambda ()
				       (interactive)
				       (mu4e-headers-mark-thread nil '(read))))
	    (local-set-key (kbd "N") 'mu4e-goto-next-thread)
	    (local-set-key (kbd "P") 'mu4e-goto-previous-thread)
	    ))

;; (define-key mu4e-compose-mode-map (kbd "C-c c") 'bjm/ivy-select-and-insert-contact)

;; helm mu - search emails and contants with helm
(use-package helm-mu
  :ensure
  :config
  (setq helm-mu-append-implicit-wildcard t))

(eval-after-load 'mu4e
  '(progn
     (define-key mu4e-view-mode-map (kbd "C-C C-C") 'org-capture)
     ;; (define-key mu4e-main-mode-map "s" 'helm-mu)
     ;; (define-key mu4e-headers-mode-map "s" 'helm-mu)
     ;; (define-key mu4e-view-mode-map "s" 'helm-mu)
     (define-key mu4e-search-minor-mode-map "s" 'helm-mu))
  )

;; mu conversations - show threads differently
;; (straight-use-package 'mu4e-conversation)
;; (global-mu4e-conversation-mode)

;; OrgMsg
(straight-use-package
 '(emacs-htmlize :type git :host github :repo "hniksic/emacs-htmlize"))
;; (straight-use-pacakge 'emacs-htmlize)
(straight-use-package 'org-msg)
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-greeting-fmt "\nHi %s,\n\n"
      org-msg-greeting-name-limit 3
      org-msg-default-alternatives '(text html)
      org-msg-convert-citation t
      org-msg-signature "

Thanks,
#+begin_signature
Shay
#+end_signature")
(setq org-msg-convert-citation t)
(org-msg-mode)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(provide 'init-email)
;;; init-email.el ends here
