;;; init-work.el --- key bindings and settings which are related to work -*- lexical-binding: t -*-


;;; Code:

(defun my/choose-from-patches ()
  "Open a file from my patches directory"
  (interactive)
  (let* ((patches-dir-files-w-attr
	  (cdr (cdr (directory-files-and-attributes "~/workspace/patches"))))
	 ; sort patches my last modified
	 (patches-dir-files-sorted (mapcar #'car (sort patches-dir-files-w-attr
						       #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
	 (patch-dir (ivy-read "Choose patch: " patches-dir-files-sorted)))
    (dired (concat "~/workspace/patches/" patch-dir))
    )
  )
(global-set-key (kbd "C-C w p") 'my/choose-from-patches)

;; (defun transpose-words-backwards ()
  ;; "Test function"
  ;; (interactive)
  ;; (message "hello world"))


(defun my--choose-directory (directory-to-start-in)
  "Return a directory chosen by the user.  The user will be prompted
to choose a directory starting with `directory-to-start-in'"
  (let* ((ivy-read-prompt "Choose directory: ")
         (counsel--find-file-predicate #'file-directory-p)
         (default-directory directory-to-start-in)
         (selected-directory
          (ivy-read
           ivy-read-prompt
           #'read-file-name-internal
           :matcher #'counsel--find-file-matcher)))
    selected-directory))

;; (my--choose-directory "~/workspace/patches/")
(provide 'init-work)

;;; init-work.el ends here
