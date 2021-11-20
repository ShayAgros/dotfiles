;; Initialize projectile, a plugin to manage projects

(straight-use-package 'projectile)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'helm-projectile)
(straight-use-package 'counsel-projectile)

(projectile-global-mode)
;; (setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)

;; (setq helm-ag-insert-at-point 'symbol)

(defun open-local-file-projectile (&optional invalidate-cache)
  "Projectile action function, open dired at project by default"
  (let* ((project-root (projectile-acquire-root)))
    (dired project-root)
    ))

(setq projectile-switch-project-action 'project-find-file)

(helm-projectile-on)
;; (counsel-projectile-mode)

;; :config(progn (bind-key "C-c
 ;; C-e" 'helm-ag-edit helm-map) (bind-key "<right>"
 ;; 'helm-execute-persistent-action helm-ag-map) (bind-key "<right>"
 ;; 'helm-execute-persistent-action helm-map) ))

(provide 'init-projectile)
