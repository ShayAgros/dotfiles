;;; Filter functions for received emails. This contains automatic actions that should be
;;; taken for received emails.
;;; Filtering through mu4e seems to lack features at best and might not be the most optimal
;;; way of handling it.
;;;
;;; So all functions here are experimental

(defvar mu4e-find-new-messages-command-template
  "mu find maildir:/gmail/ flag:unread %s --format=sexp 2>/dev/null")

(defun refile-electricity-bill-emails ()
  (let* ((cmd (format mu4e-find-new-messages-command-template "from:iec.co.il"))
	 (res (concat "(list" (shell-command-to-string cmd) ")"))
	 (msgs (car (read-from-string res))))
    (unless (equal '(list) msgs)
      (dolist (msg msgs)
	(when-let ((docid (mu4e-message-field msg :docid))
                   (maildir (funcall mu4e-refile-folder msg)))
          (mu4e~proc-move docid maildir)))))
  )

(defun mu4e-refile-messages ()
  (let* ((cmd mu4e-find-new-messages-command)
         (res (concat "(list" (shell-command-to-string cmd) ")"))
         (msgs (car (read-from-string res))))
    (unless (equal '(list) msgs)
      (dolist (msg msgs)
        (when-let ((docid (mu4e-message-field msg :docid))
                   (maildir (funcall mu4e-refile-folder msg)))
          (mu4e~proc-move docid maildir))))))

(provide 'email-filters)
