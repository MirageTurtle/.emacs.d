;;; init-utils.el

(defun print-elements-of-list (list)
  ;; Or maybe use cl-prettyprint?
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

;;; https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun mt/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
   Activate this advice with:
   (advice-add 'message :before 'mt/ad-timestamp-message)
   Deactivate this advice with:
   (advice-remove 'message 'mt/ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
	(with-current-buffer "*Messages*"
	  ;; Goto the end of buffer
	  (goto-char (point-max))
	  (if (not (bolp))
	      (newline))
	  ;; insert the timestamp
	  (insert (format-time-string "[%Y-%m-%d %T.%3N] "))))))

(advice-add 'message :before 'mt/ad-timestamp-message)

;; https://github.com/roife/.emacs.d/blob/262ed04f3e3a04eaaf5bf728b827691a07079b3f/core/init-util.el#L59
(defmacro defadvice! (symbol arglist &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &rest [WHERE PLACES...] BODY\)"
  (declare (indent defun))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defun mt/read-file-content (filepath)
  "Read the content of FILEPATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (string-trim (buffer-string))))

(provide 'init-utils)

;;; init-utils.el ends here.
