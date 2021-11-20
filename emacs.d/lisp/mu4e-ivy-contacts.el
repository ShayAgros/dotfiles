;; Choose contacts using ivy (counsel). Code taken from
;; http://pragmaticemacs.com/emacs/even-better-email-contact-completion-in-mu4e/

;;need this for hash access
(require 'subr-x)

;; Make Amazon contacts first in a list
(defun sa/mu4e-contacts-amazon-first ()
  "Returns a list of all mu4e contacts but with Amazon emails first."
  (let (amazon-contacts other-contacts)
    (dolist (contact (hash-table-keys mu4e~contacts))
      (cond ((string-match "@amazon.com" contact)
	     (add-to-list 'amazon-contacts contact))
	    (t
	     (add-to-list 'other-contacts contact)
	     )))
    (cl-concatenate 'list amazon-contacts other-contacts))
  )

;; code from https://github.com/abo-abo/swiper/issues/596
(defun bjm/counsel-email-action (contact)
  (with-ivy-window
    (insert contact)))

;; bind comma to launch new search
(defvar bjm/counsel-email-map
  (let ((map (make-sparse-keymap)))
    (define-key map "," 'bjm/counsel-email-more)
    map))

(defun bjm/counsel-email-more ()
  "Insert email address and prompt for another."
  (interactive)
  (ivy-call)
  (with-ivy-window
    (insert ", "))
  (delete-minibuffer-contents)
  (setq ivy-text ""))

;; ivy contacts
;; based on http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/
(defun bjm/ivy-select-and-insert-contact (&optional start)
  (interactive)
  ;; make sure mu4e contacts list is updated - I was having
  ;; intermittent problems that this was empty but couldn't see why
  (mu4e~request-contacts-maybe)
  (let ((eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t)))
        ;; append full sorted contacts list to favourites and delete duplicates
        (contacts-list
         (delq nil (delete-dups (sa/mu4e-contacts-amazon-first)))))

    ;; only run if we are in the headers section
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
           (start
            (or start
                (save-excursion
                  (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                  (goto-char (match-end 0))
                  (point))))
           (initial-input (buffer-substring-no-properties start end)))

      (delete-region start end)

      (ivy-read "Contact: "
                contacts-list
                :re-builder #'ivy--regex
                :sort nil
                :initial-input initial-input
                :action 'bjm/counsel-email-action
                :keymap bjm/counsel-email-map)
      ))))

(provide 'mu4e-ivy-contacts)
