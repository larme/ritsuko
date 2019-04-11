;;;; journal.lisp

(in-package #:ritsuko)

(defvar *journal-path* "~/.journal")

(defun load-lines ()
  (uiop:read-file-lines *journal-path*))

(defun split-line (line)
  (let* ((sep " | ")
	 (splited (str:split sep line))
	 (head (str:trim (car splited)))
	 (body (str:trim (str:join sep (cdr splited)))))
    (cons head body)))

(defun process-head (datetime-str)
  (let ((ts (substitute #\T #\Space datetime-str :count 1)))
    (local-time:parse-timestring (concatenate 'string ts ":00+08:00"))))

(defun comment-body? (body)
  (or (str:starts-with? "//" body)
      (str:starts-with? ";;" body)))

(defun process-body (body)
  (if (comment-body? body)
      nil
      (let ((tags (->> body
		       str:words
		       (remove-if-not (lambda (word)
					(str:starts-with? "@" word)))
		       (remove-if (lambda (word)
				    (string= "@" word)))
		       (mapcar (lambda (tag)
				 (intern (string-upcase (subseq tag 1))
					 :keyword))))))
	(if (member :comment tags)
	    nil
	    tags))))

(defun process-head-and-body (pair)
  (let* ((h (car pair))
	 (b (cdr pair))
	 (h* (process-head h))
	 (b* (process-body b)))
    (if (and h* b*)
	(cons h* b*)
	nil)))

(defun process-line (line)
  (->> line
       split-line
       process-head-and-body))

(defun process-lines (lines)
  (->> lines
       (remove-if #'str:empty?)
       (mapcar #'process-line)
       (remove-if #'null)))

(defun time-of (entry)
  (car entry))

(defun tags-of (entry)
  (cdr entry))

(defun comment? (entry)
  (if entry
      t
      nil))

(defun load-journal ()
  (stable-sort (process-lines (load-lines))
	       #'<
	       :key (lambda (pair)
		      (local-time:timestamp-to-universal (time-of pair)))))
