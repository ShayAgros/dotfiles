;;; init-email.el --- mail client
;;; Commentary:
;;; This package includes all configurations to set mu4e + smtp
;;; with Gmail and Amazon email
;;; Code:

;; Installs mu4e and configures a basic setup
(require 'email-backend-setup)

(defun bms/toggle-mu4e-gnus ()
  (interactive)
  "Toggle between old mu4e view mode and gnus-view-mode."
  (if (equal mu4e-view-use-gnus t)
      (setq mu4e-view-use-gnus nil)
    (setq mu4e-view-use-gnus t))
  (mu4e-view-refresh))

;; View email in a new buffer
(defun ed/preview-some-mail-at (msg)
  (interactive)
  (let ((path (mu4e-message-field msg :path)))
    (call-process
     "mu" nil
     (switch-to-buffer (generate-new-buffer "*mail preview*") t)
     t "view" (expand-file-name path)))
  (with-current-buffer "*mail preview*"
    (goto-char (point-min))
    (mu4e~fontify-cited)
    (mu4e~fontify-signature)
    (while (re-search-forward "^\\(\\w+:\\) \\(.*\\)$" nil t)
      (let ((key (match-string 1))
            (value (match-string 2)))
        (beginning-of-line)
        (delete-region (point) (line-end-position))
        (insert (concat (propertize key 'face 'mu4e-header-key-face) " "))
        (if (or (equal key "From:")
                (equal key "To:"))
            (insert (propertize value 'face 'mu4e-special-header-value-face))
          (insert (propertize value 'face 'mu4e-header-value-face)))))
    (forward-line)
    (beginning-of-line)
    (insert "\n")
    (read-only-mode)
    (local-set-key (kbd "q") #'kill-this-buffer)))


;; Choose contacts using ivy-mode
;; (require 'mu4e-ivy-contacts)

;; (define-key mu4e-compose-mode-map (kbd "C-c c") 'bjm/ivy-select-and-insert-contact)

;; helm mu - search emails and contants with helm
(use-package helm-mu
  :ensure
  :config
  (setq helm-mu-append-implicit-wildcard t)
  (define-key mu4e-compose-mode-map (kbd "C-C c") 'helm-mu-contacts))

(eval-after-load 'mu4e
  '(progn
     (define-key mu4e-view-mode-map (kbd "C-C C-C") 'org-capture)
     (require 'email-mailing-lists)
     (require 'email-filters)
     )
  )

;; mu conversations - show threads differently
;; (straight-use-package 'mu4e-conversation)
;; (global-mu4e-conversation-mode)

;; OrgMsg
(straight-use-package
 '(emacs-htmlize :type git :host github :repo "hniksic/emacs-htmlize"))
;; (straight-use-pacakge 'emacs-htmlize)
(use-package org-msg
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-default-alternatives '(text html)
	org-msg-convert-citation t
	org-msg-signature "

Thanks,
#+begin_signature
Shay
#+end_signature")
  (org-msg-mode))

(define-key org-msg-edit-mode-map (kbd "C-C c") 'helm-mu-contacts)

(add-to-list 
 'org-capture-templates
 '("p" "Add to related mails in project dir" entry (file my-add-related-mail-thread-to-patch)
   "* %?\nSENT-IN: %:date\n%a\n"))

(provide 'init-email)
;;; init-email.el ends here
