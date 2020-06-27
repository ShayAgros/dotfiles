
(setq inhibit-startup-message t)
(setq vc-follow-symlinks nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package counsel
  :ensure t)

(use-package auctex
 :defer t
 :ensure t
 :config
 (progn
   (setq TeX-auto-save t) 
   (setq TeX-parse-self t)
   (setq TeX-save-query nil)))

(use-package smartparens
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package projectile
  :ensure t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq TeX-save-query nil)
(setq TeX-source-correlate-method 'synctex)
;(TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)
(setq TeX-view-program-selection (quote ((output-pdf "Zathura") (output-dvi "xdvi"))))
; When formatting a column, make lines 100 chars long
(setq fill-column 100)

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )

; Key bindings
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\C-ca" 'org-agenda)


; Mail client

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'smtpmail)

; smtp
; We start by setting a single account
(setq message-send-mail-function 'smtpmail-send-it
      ;smtpmail-starttls-credentials '(("mail.cock.li" 587 nil nil))
      user-mail-address "goatlygoat@cock.li"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-user "goatlygoat@cock.li"
      smtp-local-domain "cock.li"
      smtpmail-default-smtp-server "mail.cock.li"
      smtpmail-smtp-server "mail.cock.li"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Mail"))

(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-trash-folder  "/Trash")

; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/.mbsyncrc -a"
      mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 120
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"        . ?i)
         ("/Sent Items"   . ?s)
         ("/Trash"        . ?t)
         ("/Drafts"       . ?d)))

;; show images
(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; spell check
(add-hook 'mu4e-compose-mode-hook
        (defun my-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

(autoload 'notmuch "notmuch" "notmuch mail" t)

;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-full-name "Shay Agroskin")

;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/Mail/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/Mail/")

; notmuch hooks
(add-hook 'notmuch-message-mode-hook
        (defun my-notmuch-do-compose-stuff ()
           "My settings for message composition."
           (set-fill-column 72)
           (flyspell-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Multimedia/Calendars/life.org" "~/Multimedia/Calendars/Work.org" "~/Multimedia/Calendars/Computer.org" "~/Multimedia/Calendars/Technion.org")))
 '(package-selected-packages
   (quote
    (projectile org-plus-contrib notmuch smartparens auctex autex counsel swiper which-key try use-package)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
