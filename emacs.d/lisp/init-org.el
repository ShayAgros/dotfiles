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

(provide 'init-org)

;;; init-org.el ends here


