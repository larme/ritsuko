;;;; ritsuko.lisp

(in-package #:ritsuko)

(defconstant +vertical-margin+ 50)
(defconstant +horizontal-margin+ 50)

;; bar width and height
(defconstant +bar-width+ 50)
(defconstant +bar-height+ 600)

;; horizontal space between each bar. Then the width of png can be calculated
(defconstant +word-space+ 20)
(defconstant +canvas-width+ (+ (* 2 +horizontal-margin+)
			       (* 31 +bar-width+)
			       (* 30 +word-space+)))

;; vertical space 
(defconstant +line-space+ 40)

;; global state for number of months
(defvar *month-num* 4)

;; color
(defconstant +color-mapping+
  '((:sleep 0 0 1 0.5)
    (:good 0 1 0 0.5)
    (:work 1 0 0 0.5)
    (:bad 0 0 0 0.5)
    (:rest 0.5 0.5 0.5 0.5)
    (:default 0.5 0.5 0.5 0.5)))

(defun get-color (name)
  (let ((query-res (assoc name +color-mapping+)))
    (if query-res
	(cdr query-res)
	(get-color :default))))

(defun cal-canvas-height ()
  (+ (* 2 +vertical-margin+)
     (* *month-num* +bar-height+)
     (* (1- *month-num*) +line-space+)))


(defun cal-bar-origin (month-idx day-idx)
  (let* ((x-num (1- day-idx))
	 (bar-x-size (* x-num +bar-width+))
	 (space-x-size (* x-num +word-space+))
	 (x (+ bar-x-size space-x-size +horizontal-margin+))

	 (canvas-height (cal-canvas-height))
	 (y-num (- *month-num* month-idx))
	 (bar-y-size (* y-num +bar-height+))
	 (space-y-size (* y-num +line-space+))
	 ;; we actually go to upper left instead of lower left of the
	 ;; bar and then draw bar downwardly so there's one more
	 ;; +bar-height+ to add
	 (y (+ bar-y-size
	       space-y-size
	       +bar-height+
	       +vertical-margin+)))
    (cons x y)))

(defun draw-bar (month-idx day-idx partition-info)
  (let* ((coord (cal-bar-origin month-idx day-idx))
	 (x (car coord))
	 (y (cdr coord)))

    ;; translate coordinates and invert y-axis
    (translate x y)
    (scale 1 -1)

    ;; draw whole bar border
    (set-rgba-stroke 0 0 0 1)
    (rectangle 0 0 +bar-width+ +bar-height+)
    (stroke)
    (set-rgba-stroke 1 1 1 0)

    ;; draw time partitions
    (let* ((idxs (mapcar #'car partition-info))
	   (l1 (cons 0.0 idxs))
	   (l2 (append idxs '(1.0)))
	   (color-info (cons nil (mapcar #'cadr partition-info)))
	   (partitions (mapcar #'list l1 l2 color-info)))
      (loop
	for part in partitions
	do (let* ((start-portion (nth 0 part))
		  (start (round (* +bar-height+
				   start-portion)))
		  (end-portion (nth 1 part))
		  (end (round (* +bar-height+
				 end-portion)))
		  (color (get-color (nth 2 part))))
	     (when color
	       (apply #'set-rgba-fill color))
	     (rectangle 0 start +bar-width+ (- end start))
	     (fill-path))))
    
    ;; reset coordinates
    (scale 1 -1)
    (translate (- x) (- y))))

(defun draw-test ()
  (let ((canvas-width +canvas-width+)
	(canvas-height (cal-canvas-height)))
    (with-canvas (:width canvas-width :height canvas-height)
      (let ((step (/ pi 7)))
	;; (translate 10 80)
	(set-rgb-fill 1 1 1)
	(clear-canvas)
	(loop
	  for month from 1 to *month-num*
	  do (loop
	       for day from 1 to 31
	       do (draw-bar month day
			    `((0.1 :sleep)
			      (0.3 :work)
			      (0.4 nil)
			      (0.45 :rest)
			      (0.5 :good)
			      (0.7 :bad)))))
	(save-png "~/x/tmp/test.png"))))  )
