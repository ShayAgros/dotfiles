;;; init-recentf.el --- Use emacs infrastructure to remember previous opened files -*- lexical-binding: t -*-
;;; Commentary:
 ;;; Code:

;; Remember the files I access across sessions
;; (recentf-mode 1)

;; internal package, no need to run :ensure
(use-package recentf
  :config
  (setq
   recentf-max-menu-items 400
   recentf-max-saved-items 400)
  :bind
  (("\C-x \C-r" . 'counsel-recentf))
  :after (counsel)
  )

(provide 'init-recentf)
;;; init-recentf.el ends here
