;;;; ritsuko.lisp

(in-package #:ritsuko)

(defconstant +one-day+ (* 24 60 60))

(defvar *color-stack* (list :default))

(defun push-color (color)
  (setq *color-stack* (cons color *color-stack*))
  color)

(defun pop-color ()
  (let ((color (car *color-stack*)))
    (setq *color-stack* (cdr *color-stack*))
    color))

(defun get-current-color ()
  (car *color-stack*))

(defun get-previous-color ()
  (let ((color (cadr *color-stack*)))
    (if color
	color
	:default)))

(defun exec-action (action)
  (when action
    (if (functionp action)
	(funcall action)
	(push-color action)))
  (get-current-color))

(defun tags->action (tags)
  (cond
    ((member :start tags) (cond
			    ((member :work tags) :work)
			    ((member :bad tags) :bad)
			    ((member :rest tags) :rest)
			    (t :good)))
    ((member :sleep tags) :sleep)
    ((member :pause tags) (lambda ()
			    (let ((pc (get-previous-color)))
			      (push-color pc))))
    ((member :resume tags) #'pop-color)
    ((member :wakeup tags) #'pop-color)
    ((member :stop tags) #'pop-color)

    (t nil)))

(defun tags->color (tags)
  (exec-action (tags->action tags)))

(defun time->date (time)
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (local-time:timestamp-to-universal time))
    (list (+ (* year 100) month) date)))

(defun time->ratio (time)
  (let* ((now (local-time:adjust-timestamp time
		(set :nsec 0)))
	 (start-of-day (local-time:adjust-timestamp now
			 (set :hour 0)
			 (set :minute 0)
			 (set :sec 0)))
	 (time-in-secs (mapcar #'local-time:timestamp-to-universal
			       (list now start-of-day))))
    (/ (apply #'- time-in-secs) +one-day+)))

(defun process-entry (entry)
  (let ((tags (tags-of entry))
	(time (time-of entry)))
    (format t "~a ~a~&" time *color-stack*)
    (list (time->date time)
	  (time->ratio time)
	  (tags->color entry))))

(defun load-entries ()
  (mapcar #'process-entry (load-journal)))

(defun cal-month->idx (entires)
  (let* ((%months (mapcar #'caar entires))
	 (months (sort (remove-duplicates %months) #'<))
	 (idxs (loop
		 for idx from 1 to (length months)
		 collect idx)))
    (mapcar #'cons months idxs)))

(defun prepare-draw-data (entries)
  (let* ((month->idx (cal-month->idx entries))
	 (month-num (length month->idx))
	 (grouped-entries (group-by entries)))
    (values month-num
	    (loop
	      for (date . draw-info) in grouped-entries
	      collect (let* ((month (car date))
			     (month-idx (cdr (assoc month
						    month->idx)))
			     (day-idx (cadr date))
			     (draw-info (stable-sort draw-info
						     #'<
						     :key #'car)))
			(list month-idx day-idx draw-info))))))

(defun make-graph ()
  (let ((*color-stack* (list :default)))
    (multiple-value-bind (month-num draw-args)
	(prepare-draw-data (load-entries))
      (let* ((*month-num* month-num)
	     (canvas-width +canvas-width+)
	     (canvas-height (cal-canvas-height)))
	(print *month-num*)
	(with-canvas (:width canvas-width :height canvas-height)
	  (set-rgb-fill 1 1 1)
	  (clear-canvas)
	  (loop
	    for draw-arg in draw-args
	    do (apply #'draw-bar draw-arg))
	  (save-png "~/x/tmp/test.png"))))))
