; emacs main config file

; General settings
;; Set default font
(add-to-list 'default-frame-alist
	     '(font . "FiraCode Nerd Font Mono-11"))
;; Don't display splash screen
(setq inhibit-startup-message t)

;; Follow symlinks
(setq vc-follow-symlinks nil)

;; Ask y or n instead of yes-or-no everywhere
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove toolbar in gui mode
(tool-bar-mode -1)

;; Add ~/.emacs/lisp to PATH
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set file for custom (automatic) configurations and load it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

; Load emacs packages

;; Basic packages (which aren't used by others) 
;;; configure MELPA
(require 'pkg-mng)
;;; gruvbox theme
(require 'theme-cfg)
;;; setup flyspell
(require 'init-spelling)
;;; init ivy completion (loads counsel and ivy-xref)
(require 'init-ivy)
;;; init par edit (auto closing paranthesis and stuff)
;; http://mumble.net/~campbell/emacs/paredit.html
(require 'init-paredit)
;; Init completion engine
(require 'init-company)
;; parentheses highlighting and stuff
(require 'init-highlighting)

;; Advance packges (which require the basic packages to be loaded)

;;; email mu4e + mbsync
(require 'init-email)
