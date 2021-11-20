;;; init-snippets.el --- Configure Yasnippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after (yasnippet))

;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets" ;; personal snippets
;;         "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"
;;         ))
;; ;; (require 'yasnippet)

;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)

(global-set-key "\C-o" 'aya-open-line)

(defun aya-open-line ()
  "Call `open-line', unless there are abbrevs or snippets at point.
In that case expand them.  If there's a snippet expansion in progress,
move to the next field. Call `open-line' if nothing else applies."
  (interactive)
  (cond ((expand-abbrev))

        ((yas--snippets-at-point)
         (yas-next-field-or-maybe-expand))

        ((ignore-errors
           (yas-expand)))

        (t
         (open-line 1))))

(yas-global-mode 1)

(provide 'init-snippets)
