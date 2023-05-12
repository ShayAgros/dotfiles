;;; Configure functions and configurations for handling mailing list
;;; patchset and discussions

;; -- External plugins ----------------------------------------------------

; colorize patch

;; TODO: One these two seems redundant. Both display patches and diffs. Seems like
;; the latter is the future developemnt of the former

;; (require 'mu4e-patch)
;; (add-hook 'mu4e-view-mode-hook #'mu4e-patch-highlight)

(straight-use-package
 '(message-view-patch :type git :host github :repo "seanfarley/message-view-patch"))
(add-hook 'gnus-part-display-hook 'message-view-patch-highlight)

;; -- Custom functions -----------------------------------------------------
(setq shayagr~mu4e-patchsets-plist '())

(defun shayagr~mu4e-add-patchset-for-review-list (&optional msg)
  "When reviewing a patchset in emacs (mu4e) itself calling this function would do the following:
1. Create a dedicated buffer for the patchset thread to which the user can return to continue reviewing
2. Add the patchset to shayagr~mu4e-patchsets-plist with information on which patches were reviewed"
  (interactive)
  (unless (or (not (null msg))
	      (eq major-mode 'mu4e-view-mode)
	      (eq major-mode 'mu4e-headers-mode))
    (error "Need to view message thread or specific message to call this function"))
  (let* ((msg (or msg (mu4e-message-at-point)))
	 (msg-id (mu4e-message-field msg :message-id))
	 (ts (current-time))
	 (cover-letter (shayagr~mu4e-find-msg-thread-cover-letter msg-id))
	 (cl-msgid (mu4e-message-field cover-letter :message-id))
       	 (msg-plist
	  (list :msg-id cl-msgid
		:subject (mu4e-message-field cover-letter :subject)
		:path (mu4e-message-field cover-letter :path)
		:reviewed t
		:added-ts ts)))
    ;; add thread to review list (if it's already there only the timestamp changes)
    (setq shayagr~mu4e-patchsets-plist (lax-plist-put shayagr~mu4e-patchsets-plist cl-msgid msg-plist))
    ;; (princ shayagr~mu4e-patchsets-plist)
    ))

(defun prop-value (plist)
  "get values of a plist"
  (let ((pl (cdr plist))
	(vals ()))
    (while pl
      (push (car pl) vals)
      (setq pl (cddr pl)))
    (nreverse vals)))

(defun shayagr~mu4e-review-prev-patchset ()
  "Continue reviewing patchset that was added to shayagr~mu4e-patchsets-plist"
  (interactive)
  (let* ((patchset-list (prop-value shayagr~mu4e-patchsets-plist))
	 ;; Create list of patchsets subjets and link them with the message object itself
	 (thread-selections (mapcar
			     (lambda (msg-plist)
			       (propertize (plist-get msg-plist :subject) 'msg msg-plist))
			     patchset-list))
	 (patch (ivy-read
		 "Choose patchset to review: " thread-selections))
	 )
    (princ (get-text-property 0 'msg patch))
    ;; Display thread in mu4e
    (let* ((msg (get-text-property 0 'msg patch))
	  (msgid (plist-get msg :msg-id))
	  (mu4e-search-threads t)
          (mu4e-headers-include-related t))
        (mu4e-search
         (format "msgid:%s" msgid)
         nil nil nil
         msgid (and (eq major-mode 'mu4e-view-mode)
                    (not (eq mu4e-split-view 'single-window)))))
    ;; (message "list: %s\n" thread-selections)
    ))

(defun shayagr~mu4e-find-msg-thread-cover-letter (msgid)
  "Find the cover letter message of a thread containing msgid

To avoid doing any asynchronous tasks by accessing mu-server this
function executes mu through shell.

In some cases the cover letter message might not be found by
mu (e.g. when it's not locally available), so the function checks
whether the oldest available message contains in-reply-to
field. If it does, it's assumed that the real first message of
the thread is not available and an error is raised"
  (let* (oldest_msg
	 (query-command (format
			 "%s find --include-related --sortfield=date --skip-dups --fields l -n 1 --format sexp msgid:%s"
			 mu4e-mu-binary
			 msgid))
	 )
    ;; Execute command and save its output
    (setq oldest_msg (car (read-from-string
			   (decode-coding-string
			    (shell-command-to-string query-command)
			    'utf-8 t))))
    ;; TODO: You can execute a web search for the email thread in case you don't
    ;; have the first thread message. Worth checking how frequent that happens
    (unless (string-empty-p (mu4e-msg-field oldest_msg :in-reply-to))
	(error "Couldn't find cover-letter message"))
    oldest_msg
    ))

(defun mu4e-apply-to-net-next (msg)
  "Apply `MSG' as a git patch to net-next directory in ~/workspace/net-next"
  (let ((default-directory "~/workspace/net-next/"))
    (shell-command
     (format "git am -3 %s"
             (shell-quote-argument (mu4e-message-field msg :path))))))

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

(defun shayagr-search-all-results ()
  "Search for an expression but don't limit results"
  (interactive)
  (let ((mu4e-headers-full-search 3000)
	(mu4e-headers-show-threads nil))
    (mu4e-headers-search)))

(defun mu4e-goto-previous-thread ()
  "Jump the the previous thread or the head of the current thread"
  (interactive)
  ;; TODO: You don't really need to evaluate this data to find
  ;; thread start
    (let* ((msg (mu4e-message-at-point))
	   (thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
      (mu4e-headers-find-if-next
       (lambda (mymsg)
	 (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
	       (my-thread-path (mu4e~headers-get-thread-info mymsg 'path)))
	   (string= (concat my-thread-id ":z") my-thread-path) ;; thread's first message
	   )
	 )
       ;; search backward
       t)
      )
    )

(defun shayagr~mu4e-get-thread-cover-msg ()
  "Get the msg plist object of the first patch in the the current patchset"
  (save-excursion (mu4e-headers-find-if
		   (lambda (mymsg)
		     (let ((my-thread-id (mu4e~headers-get-thread-info mymsg 'thread-id))
			   (my-thread-path (mu4e~headers-get-thread-info mymsg 'path)))
		       (string= (concat my-thread-id ":z") my-thread-path) ;; thread's first message
		       )
		     )
		   ;; search backward
		   t)
		  (mu4e-message-at-point))
  )

(defun mu4e-get-netdev-mbox-file ()
  "Download the mbox file of the patchset under cursor"
  (interactive)
  (let* ((cover-msg (my-mu4e-get-thread-cover-msg))
	 (thread-id (mu4e-message-field cover-msg :message-id)))
    (setq curl_command (format
			"curl -Ls 'https://patchwork.kernel.org/project/netdevbpf/cover/%s'  | grep -oE '/series/[0-9]+/mbox/' | xargs -I {} wget --quiet -O series.patch https://patchwork.kernel.org{}"
			thread-id))
    (kill-new curl_command)
    (print curl_command)
    ))

(defun shayagr~mu4e-apply-thread-to-net-next ()
  "Apply a patchset on net-next"
  (interactive)
  (let* ((msg mu4e~view-message)
	 (default-directory "~/workspace/net-next/")
	 (msgid (mu4e-message-field msg :message-id))
	 (date (format-time-string mu4e-headers-long-date-format (mu4e-message-field msg :date))))

    (message last_commit)))

(defun my-add-related-mail-thread-to-patch ()
  "Chooses a project and adds this mailthread to its related emails"
  (let* ((patches-dir-files
	  (cdr (cdr (directory-files "~/workspace/patches"))))
	 (patch-dir (ivy-read "Choose patch: " patches-dir-files)))
    (concat "~/workspace/patches/" patch-dir "/related_emails.org"))
  )

;; Add marking patchset as being reviews to action list
(add-to-list 'mu4e-view-actions '("Include patchset in review list" . shayagr~mu4e-add-patchset-for-review-list) t)
(add-to-list 'mu4e-headers-actions '("Include patchset in review list" . shayagr~mu4e-add-patchset-for-review-list))

(provide 'email-mailing-lists)
