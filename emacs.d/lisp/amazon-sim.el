(straight-use-package
 '(request :type git :host github :repo "tkf/emacs-request"))
(require 'request)

(straight-use-package
 '(plz :type git :host github :repo "alphapapa/plz.el"))
(require 'plz)

(defun my-wat-rel-timestamp (&optional year month day)
  "Receives a plist matching the values expected by make-decoded-time and returns a relative
time string to current time with anything smaller than a week zeroed (i.e. defaults to start of the current week always
The returned time has the format of year-month-dayT23:59:59+03:00 because this it the string WAT expects"
  (format-time-string "%G-%m-%dT23:59:59+03:00"
		      (encode-time (decoded-time-add (decode-time)
						     (make-decoded-time :month month))))
  )

(defun fetch_wat_issues (&optional week)
  "Fetch to current buffer the WAT tickets of the past two months"
  (interactive)
  (with-output-to-temp-buffer "wat-output"
      (let* ((url-cookie-file (expand-file-name "~/.midway/cookie"))
	     (wat-domain "https://wat.annapurna.aws.a2z.com")
	     (from (my-wat-rel-timestamp :month -2))
	     (to (my-wat-rel-timestamp))
	     (wat-url (format "%s/wat?to=%s&from%s" wat-domain to from)))
	(print wat-url)))
  )

;; (with-current-buffer (get-buffer-create "temp-buffer")
;;   (delete-region (point-min) (point-max))
;;   (insert-file-contents (expand-file-name "~/.midway/cookie"))
;;   (replace-regexp-in-region "#HttpOnly_\\(\\.?\\)" "")
;;   (view-buffer-other-window (current-buffer))
;;   (write-region nil nil "/tmp/modified_cookies" nil )
;;   )


;; (with-current-buffer (url-retrieve "https://www.google.com" (lambda (status)
							      ;; (message "Request finished"))) )
;; (with-current-buffer
    ;; (let* (url-cookie-file (expand-file-name "~/.midway/cookie"))
      ;; (url-retrieve-synchronously "https://wat.annapurna.aws.a2z.com/wat?to=2022-12-10T23:59:59+03:00&from2022-10-10T23:59:59+03:00"))
  ;; (switch-to-buffer (current-buffer)))


;; (let* ((midway_cookie ())))
;; (plz :get "www.google.com")

;; (let* ((request--curl-cookie-jar "~/.midway/cookie"))
;;   (request "https://wat.annapurna.aws.a2z.com/wat?to=2022-09-25T23%3A59%3A59%2B03%3A00&from=2022-09-12T00%3A00%3A00%2B03%3A00"
;;     :parser 'json-read
;;     :success (cl-function
;;               (lambda (&key data &allow-other-keys)
;; 		(let ((tickets (assoc 'tickets data)))
;; 		  (dolist (ticket tickets)		  
;; 		    ;; (message "week: 4")
;; 		    ))))
;;     )
;;   )
