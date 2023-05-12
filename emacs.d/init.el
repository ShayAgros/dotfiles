; emacs main config file

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done)))

; General settings
;; Set default font
(add-to-list 'default-frame-alist
	     '(font . "FiraCode Nerd Font Mono-11"))
;; Don't display splash screen
(setq inhibit-startup-message t)

;; configure garbase collection to run only when idle and increase its threashold
(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'after-init-hook
          #'(lambda ()
              (setq gc-cons-threshold (* 100 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;; Follow symlinks
(setq vc-follow-symlinks nil)

;; Ask y or n instead of yes-or-no everywhere
(defalias 'yes-or-no-p 'y-or-n-p)

;; This should stop flickering in terminal mode.
;; It seems kinda important but I can't find why
(setq recenter-redisplay nil)

;; remove toolbar in gui mode
(tool-bar-mode -1)

;; Add ~/.emacs/lisp to PATH
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set file for custom (automatic) configurations and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Save emacs information between sessions
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring projectile-project-command-history))
(setq savehist-file "~/.emacs.d/tmp/savehist")
(savehist-mode 1)

;; Stop making backup files in the same directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Stop making auto-save files in the same directory
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))

;; (setq-default show-trailing-whitespace t)

;; check spelling by default
(flyspell-mode)

;; Don't truncate eval output in echo area
(setq eval-expression-print-length nil)

;; Remember file location
(require 'saveplace)
(save-place-mode)

;; Push clipboard into kill ring before pushing a new value to it
(setq save-interprogram-paste-before-kill t)

;; Push kill ring content to clipboard
(setq x-select-enable-clipboard t)
;; Load emacs packages

;; Make buffer re-read from disk automatically
(global-auto-revert-mode)

;; Basic packages (which aren't used by others) 
;; configure MELPA
(require 'pkg-mng)
;; gruvbox theme
(require 'theme-cfg)
;; setup flyspell
(require 'init-spelling)
(require 'init-helm)
;; init ivy completion (loads counsel and ivy-xref)
(require 'init-ivy)
;; init par edit (auto closing paranthesis and stuff)
;; http://mumble.net/~campbell/emacs/paredit.html
(require 'init-paredit)
;; parentheses highlighting and stuff
(require 'init-highlighting)
;; project management
(require 'init-projectile)
;; display avialable keys
(require 'init-which-key)
;; setup git integration
(require 'init-git)
;; setup try package (allows to try a package)
(require 'init-try)
;; setup json
;; (require 'init-json)

;; Completion engine. Backends should come before completion engine
;; initialization

;;; setup language server
(require 'init-lsp)
;;; setup snippets
(require 'init-snippets)
;;; Init completion engine
(require 'init-company)

;; Advance packges (which require the basic packages to be loaded)

;; Org more settings (org-agenda and org-capture as well)
;; Has to be first here as some of the pacakages utilize org settings
(require 'init-org-roam)
(require 'init-org)

;; email mu4e + mbsync
;; (autoload 'mu4e "init-email")
(require 'init-email)

(require 'init-c)
;; Initialize all python related configurations
(require 'init-python)
;; Inialize all javascript related configurations
(require 'init-js)
;; An emacs framework to save recently opened files
(require 'init-recentf)

;; Work related settings
(require 'init-work)

(defface visible-mark-active ;; put this before (require 'visible-mark)
  '(
    (((type tty) (class mono)))
    (t (:background "magenta")))
  "")
(require 'visible-mark)
(global-visible-mark-mode 1) ;; or add (visible-mark-mode) to specific hooks
(setq visible-mark-max 1)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))

;; Surround text
(defun surround (begin end open close)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (when (string= close "")
    (setq close open))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))
(global-set-key "\C-cs" 'surround)

(setq compilation-ask-about-save nil)

;; Stop eshell from sclling down
(add-hook 'eshell-mode-hook
          (defun chunyang-eshell-mode-setup ()
            (remove-hook 'eshell-output-filter-functions
                         'eshell-postoutput-scroll-to-bottom)))
