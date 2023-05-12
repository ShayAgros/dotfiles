;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package
 '(paredit :type git :host nil :repo "http://mumble.net/~campbell/git/paredit.git")
 ;; :hook (
	;; ((lisp-mode emacs-lisp-mode) .
	 ;; enable-paredit-mode))
 )

(add-hook 'lisp-mode-hook (lambda () (enable-paredit-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (enable-paredit-mode)))

;; (use-package paredit
  ;; :ensure
  ;; :hook (
	 ;; ((lisp-mode emacs-lisp-mode) .
	  ;; enable-paredit-mode)
	 ;; ))


;; This should make it work with displaying function's declaration
;; in mini-buffer
;;(require 'eldoc) ; if not already loaded
;;(eldoc-add-command
;; 'paredit-backward-delete
;; 'paredit-close-round)

;; Don't run par edit mode on mini buffers at all. This is just buggy
;; (add-hook 'minibuffer-setup-hook (lambda () (paredit-mode 0)))

(provide 'init-paredit)
;;; init-paredit.el ends here
