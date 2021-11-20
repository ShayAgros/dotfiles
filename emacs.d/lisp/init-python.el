;; Init required package for smooth python development

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(setq lsp-python-ms-executable
      "~/workspace/Software/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")


(provide 'init-python)
