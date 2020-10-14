;; Init required package for smooth C/C++ development

(use-package ggtags
  :ensure
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))



(require 'semantic)

;; Cache results
(global-semanticdb-minor-mode 1)
;; Re-parse idle buffers
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; (setq semantic-new-buffer-setup-functions
;;       (remove-if (lambda (buffer-setup-function)
;;                    (cl-member (car buffer-setup-function)
;;                            '(python-mode html-mode)))
;;                  semantic-new-buffer-setup-functions))

;; (remove-hook 'python-mode-hook 'wisent-python-default-setup)

(defun company-semantic-setup ()
  "Configure company-backends for company-semantic and company-yasnippet."
  (delete 'company-irony company-backends)
  (push '(company-semantic :with company-yasnippet) company-backends))

(global-ede-mode 1)

(setq ede-custom-file (expand-file-name "cc-mode-projects.el" user-emacs-directory))
(when (file-exists-p ede-custom-file)
  (load ede-custom-file))

(defun company-c-headers-setup ()
  (add-to-list 'company-backends 'company-c-headers))
