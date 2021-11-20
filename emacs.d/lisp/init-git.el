;; This file sets up git

(with-eval-after-load 'magit-log
  (put 'magit-log-select-mode 'magit-log-default-arguments
       '("-n256" "--decorate")))

(straight-use-package 'magit)

(provide 'init-git)
