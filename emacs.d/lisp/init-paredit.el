;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package paredit
  :ensure t)

(add-hook 'minibuffer-setup-hook 'enable-paredit-mode)

(add-hook 'prog-mode-hook 'paredit-mode)

(provide 'init-paredit)
;;; init-paredit.el ends here
