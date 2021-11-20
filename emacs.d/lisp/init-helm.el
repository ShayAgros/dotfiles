;; Initialize helm competion engine

(straight-use-package 'helm)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-x b") 'helm-buffers-list)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

(setq projectile-enable-caching nil)
(helm-mode 1)

(straight-use-package 'helm-ag)

(provide 'init-helm)
