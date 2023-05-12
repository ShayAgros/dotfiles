;;; init-c.el --- Configure all javascript specific packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :hook (js2-mode . (lambda ()
		      (js2-imenu-extras-mode)
		      (paredit-mode)))
  )

(use-package js2-refactor
  :straight t)

(use-package xref-js2
  :straight t)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))



(provide 'init-js)
