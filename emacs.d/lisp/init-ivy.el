;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :init
  ;; don't prefix searches
  (setq-default ivy-initial-inputs-alist '())
  :config
  (setq-default
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'fullpath
   ivy-count-format ""
   ivy-magic-tilde nil
   ivy-dynamic-exhibit-delay-ms 150)
  
  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  ;; make C-j and C-RET choose result
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer))

;; completion engine
(use-package counsel
  :ensure t
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :config
  (add-hook 'after-init-hook 'counsel-mode))

;; fuzzy sarch in file
(use-package swiper
  :ensure t
  :config
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(provide 'init-ivy)
;;; init-ivy.el ends here
