;;; init-c.el --- Configure all C specific packages -*- lexical-binding: t -*-
;;; Commentary: This includes helm-gtags, CEDET and LSP C configuration (CCLS)
;;; Code:

;; Init required package for smooth C/C++ development

;; (use-package ggtags
;;   :ensure
;;   :config
;;   (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))
;;   (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;   (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;;   (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;;   (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;;   (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;;   (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;;   (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

;; helm gtags (static tagging system)
(use-package helm-gtags
  :ensure
  :init
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t)
  :config
  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  :hook (
	 (dired-mode . helm-gtags-mode)
	 (c-mode . helm-gtags-mode)
	 (eshell-mode . helm-gtags-mode)
	 (asm-mode . helm-gtags-mode)
	 )
)

;; (remove-hook 'python-mode-hook 'wisent-python-default-setup)

;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)

;; (semantic-mode 1)

(setq ede-custom-file (expand-file-name "cc-mode-projects.el" user-emacs-directory))
(when (file-exists-p ede-custom-file)
  (load ede-custom-file))

(defun company-c-headers-setup ()
  (add-to-list 'company-backends 'company-c-headers))

(setq c-default-style "linux"
          c-basic-offset 8)

;; Configure LSP client (CCLS)
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(setq ccls-executable "~/workspace/scripts/ccls_execute.sh")

;; From the Linux coding style document. It should make emacs work
;; nicer with kernel's files

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only"))
	      ;; CEDET (emacs native support for C projects). This currently doesn't work well with LSP
	      ;; You need to either choose between them of find a way to integrate both
	      (global-ede-mode))))

;; remap ede key bidning to C-c e since C-c . is already used by org mode, and it is a headache to
;; make it work for each mode
(with-eval-after-load 'ede
  (define-key ede-minor-mode-map (kbd "C-c e")
    (lookup-key ede-minor-mode-map (kbd "C-c .")))
  (define-key ede-minor-mode-map (kbd "C-c .") nil))

(provide 'init-c)
