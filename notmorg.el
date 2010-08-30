(defvar notmorg-monthnames '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "monthnames for reading out dates from mail bodies")

(defvar notmorg-date-extractors nil
  "A list of functions of one parameter searching a body of text, returning a timestamp")

(setf notmorg-date-extractors
  (list 
   (lambda (text) (progn
	       (when (and text (string-match "\\([0-9]\\{1,2\\}\\) \\([A-Za-z]\\{3\\}\\) \\([0-9]\\{4\\}\\)" text))
		 (let ((day   (read (match-string 1 text)))
		       (month-index (position (match-string 2 text) ks-monthnames-lastfm :test #'string=))
		       (year  (read (match-string 3 text))))
		   (when month-index
		     (encode-time 0 0 0 day (+ month-index 1) year))))))))

(defun notmorg-from-tag (tag)
  "tag is string or (string todo-flag)"
  (unless (listp tag) (setf tag (list tag nil)))
  (with-temp-buffer
    (dolist (scheduled (let ((json-array-type 'list)
			     (json-object-type 'plist))
			 (json-read-from-string 
			  (shell-command-to-string (concat "notmuch show --format=json tag:" (first tag))))))
      (let* ((entry   (first (first scheduled)))
	     (body    (getf (first (getf entry :body)) :content))
	     (subject (getf (getf entry :headers) :Subject))
	     (date    (run-hook-with-args-until-success 'notmorg-date-extractors body)))
	(insert "* ")
	;; second tag is todo-flag status
	(when (second tag) (insert "TODO "))
	(insert (concat subject "\n"))
	(when (and date (time-less-p (current-time) date))
	  (insert "SCHEDULED: ")
	  (org-insert-time-stamp date)
	  (insert "\n"))
	(insert (concat body "\n"))))
    (buffer-string)))

(defun notmorg-write-file (file &rest tags)
  (let ((message (apply #'concat (mapcar (lambda (x) (notmorg-from-tag x)) tags)))
	(buffer (get-file-buffer file)))
    (if buffer
	(with-current-buffer buffer
	  (delete-region (point-min) (point-max))
	  (insert message)
	  (save-buffer))
      (with-temp-buffer
	(insert message)
	(write-file file)))))

(provide 'notmorg)