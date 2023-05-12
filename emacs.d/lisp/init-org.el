;;; init-org.el --- Some function to parse org mode files -*- lexical-binding: t -*-
;; commentary
;;; Code:

(defun shay_list_paragraphs ()
  "list all paragraphs in current org buffer"
  (interactive)

  (print (org-element-map (org-element-parse-buffer) 'paragraph
     (lambda (paragraph)
       (let ((parent (org-element-property :parent paragraph)))
	 (and (eq (org-element-type parent) 'section)
              (let ((first-child (car (org-element-contents parent))))
		(eq first-child paragraph))
              ;; Return value.
              paragraph))))))

(defun shay_predicate_todos ()
  "list all paragraphs in current org buffer"
  (interactive)

  (print (org-element-map (org-element-parse-buffer) 'item
	   (lambda (item) (not (eq (org-element-property :todo-keyword (org-element-at-point)) nil))) )
	 ))

(defun shay_list_todos_in_file ()
  "Return all todo items in a list"
  (interactive)

  (let ((todo_items '()))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (item)
	;; Is the item a TODO item ?
	(and (not (eq (org-element-property :todo-keyword item) nil))
	     (push (org-element-property :title item) todo_items))
	)
      )
    (princ todo_items)
    )
  )

(defun tt-print-headings ()
  "print all headings in current buffer (of org mode).
2019-01-14"
  (interactive)
  (with-output-to-temp-buffer "*xah temp out*"
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (item)
	;; Is the item a TODO item ?
	(and (not (eq (org-element-property :todo-keyword item) nil))
	     ;; (push (org-element-property :title item) todo_items))
	     (princ (org-element-property :title item))
	     (terpri))
	)
      )))

(defun parse_todo_in_org (org_file)
  "Print all TODO items' title in an org file"
  (setq org-ast (with-temp-buffer
                  (insert-file-contents org_file)
		  (org-mode)
		  (org-element-parse-buffer 'headline)))

    (let ((titles '())) (org-element-map org-ast 'headline
	    (lambda (item)
	      ;; Is the item a TODO item ?
	      (and (not (eq (org-element-property :todo-keyword item) nil))
		   (push (org-element-property :title item) titles)
		   (princ (org-element-property :title item))
		   (terpri))
		   )
	    )
	 ;; (mapcar (lambda (item)
		   ;; (print item))
		 ;; titles)
	 )
    )


;; (parse_todo_in_org "/tmp/test.org")

;; Org capture settings

(setq org-capture-templates '()
      ;; Export settings
      org-export-with-sub-superscripts nil
      org-export-with-toc nil)

(add-to-list
 'org-capture-templates
 '("h" "Tasks that probably should be done someday (most for habits)" entry (file+headline "~/workspace/org_tasks/nice_to_have_todos.org" "Things to improve in the general workflow")
   "* TODO [#C] %?\n"))

;; Allow to org-capture emails that I want to read later
(add-to-list
 'org-capture-templates
 '("r" "Read patch later" entry (file+headline "~/workspace/org_tasks/mailing_list_reads.org" "Emails to read")
    "* TODO [#C] %?\nSENT-IN: %:date\n%a\n"))

(add-to-list
 'org-capture-templates
 '("w" "Tasks that need to done by weekly" entry (file+headline "~/workspace/org_tasks/work_weekly_todos.org" "Work weekly todos")
   "* TODO [#A] %?\nDEADLINE: %(org-insert-time-stamp (org-read-date t t \"saturday 23:59\"))\n"))

; Add a work related todo list. If we're in an email, then link to it
(add-to-list 
 'org-capture-templates
 '("t" "Task that needs to be done for work" entry (file+headline "~/workspace/org_tasks/work_todos.org" "Work todos")
   "* TODO [#A] %?\nSCHEDULED: %^{By when this task should be done:}t\n%(if (string= last-major-mode 'mu4e-view-mode) \"%a\n\")"))

;; Capture tasks with a global key (save the last major mode. Some captures use this information)
(global-set-key (kbd "C-c o c") (lambda ()
				  (interactive)
				  (setq last-major-mode (symbol-name major-mode))
				  (org-capture)))

;; Org TODO settings
;; Define Org mode task state

(defun rasmus/remove-schedule ()
  "Remove SCHEDULED-cookie is switching state to WAITING."
  (save-excursion
    (and (equal (org-get-todo-state) "WAITING")
	 (org-get-scheduled-time (point))
	 (when (search-forward-regexp org-scheduled-time-regexp nil t)
	   (or (delete-region (match-beginning 0) (match-end 0)) t))
	 (get-buffer "*Org Agenda*")
	 (with-current-buffer "*Org Agenda*"
	   (org-agenda-redo)))))

;; Remove schedule keyword when moving a task to WAIT state
(add-hook 'org-after-todo-state-change-hook
     'rasmus/remove-schedule)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


;; Org agenda
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "<f12>") 'org-agenda-list)
(global-set-key (kbd "<f9> c") 'calendar)
(setq org-agenda-files '("~/workspace/org_tasks"))

;; TODO: This for some work no longer works since emacs doesn't
;; recognize the command org-agenda-overriding-header
;; (setq org-agenda-custom-commands
      ;; (quote ((" " "Agenda"
	       ;; ((agenda "" nil)
		;; (tags-todo "WAITING"
			   ;; ((org-agenda-overriding-header "Waiting tasks")))
		;; (tags-todo "-WAITING"
			   ;; ((org-agenda-overriding-header "Future todos"))))
	       ;; nil))))

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; start the week on Sunday
(setq org-agenda-start-on-weekday 0)

(cl-defun ap/org-set-level-faces (&key (first-parent 'outline-1))
    (require 'color)
    (require 'dash)
    (require 'org-inlinetask)
    (let ((org-level-color-list (-cycle (list "red" "orange" "yellow"))))
      (cl-flet ((modify-color (color) (thread-first color
                                        (color-desaturate-name 30))))
        (cl-loop for level from 1 to (1- org-inlinetask-min-level)
                 for face = (intern (format "org-level-%s" level))
                 for parent = (cl-case level
                                (1 (list first-parent 'highlight))
                                (t (intern (format "org-level-%s" (1- level)))))
                 for height = (cond ((= level 1) 1.3)
                                    ((<= level 4) 0.9)
                                    (t 1.0))
                 for weight = (if (<= level 8) 'bold 'normal)
                 unless (internal-lisp-face-p face)
                 do (custom-declare-face face `((t :inherit ,(intern (format "outline-%s" (1- level)))))
                                         (format "Face for Org level %s headings." (1- level)))
                 do (set-face-attribute face nil
                                        :inherit parent
                                        :foreground (modify-color (nth level org-level-color-list))
                                        :height height
                                        :weight weight
                                        :overline t)
                 collect face into faces
                 finally do (defconst org-level-faces faces)
                 finally do (setq org-n-level-faces (length org-level-faces))))))


(add-hook 'org-mode-hook 'flyspell-mode)

;; Needed to export org mode to quip using markdown export as intermidiate
(defun org-md-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat "```\n"
   (org-remove-indentation
    (org-export-format-code-default example-block info))
   "```"))

(use-package plantuml-mode
  :ensure)
;; (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
;; (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(setq plantuml-jar-path "~/workspace/Software/plantuml.jar")
(setq org-plantuml-jar-path "~/workspace/Software/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t))) ; this line activates plantuml

;; don't ask for confirmation before evaluating code blocks
(setq org-confirm-babel-evaluate nil)


;; (defface org-level-0 `((t :inherit 'variable-pitch))
  ;; (format "Face for Org level %s headings." 0))
;; (set-face-attribute (intern  "org-level-0") nil
                    ;; :inherit '(variable-pitch highlight)
                    ;; :foreground (color-desaturate-name "red" 30)
                    ;; :height 1.3
                    ;; :weight 'bold
                    ;; :overline t)
;; (internal-lisp-face-p (intern "org-level-1"))
;; (defconst org-level-faces (list (intern "org-level-0")))
;; (setq org-n-level-faces (length org-level-faces))
;; (ap/org-set-level-faces :first-parent 'variable-pitch)

;; (cl-loop for buf in (buffer-list)
         ;; collect (buffer-file-name buf))

;; (set-face-attribute 'org-agenda-overidden-header nil :foreground "red")

(provide 'init-org)

;;; init-org.el ends here
