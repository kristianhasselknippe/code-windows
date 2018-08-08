(require 'dash)
(require 'cl)
(require 'ov)

(setq cw/buffer 'nil)

(setq cw/code-window-buffer-name "code-window-buffer")

(setq cw/windows '())

(cl-defstruct cw/window buffer region-begin region-end name)

(defun cw/render-window-in-window-buffer (window)
  (with-current-buffer cw/buffer
	(ov-set (ov-insert (format "=======%s=======" (cw/window-name window))) 'face '(:foreground "#66cdaa"))
	(insert "\n")
	(insert-buffer-substring (cw/window-buffer window) (cw/window-region-begin window) (cw/window-region-end window))
	(insert "\n")
	(ov-set (ov-insert "================") 'face '(:foreground "#7fffd4"))
	(insert "\n\n")))

(defun cw/mark-window-in-owners-buffer (window)
  (with-current-buffer (cw/window-buffer window)
	(let ((window-begin (cw/window-region-begin window))
		  (window-end (cw/window-region-end window)))
	  ;;(message (format "Setting window from %d to %d" window-begin window-end))
	  ;;(let* ((window-buffer (cw/window-buffer window))
	  ;;		 (buffer-len (with-current-buffer window-buffer (buffer-size))))
	  ;;	(message (format "Buffer length: %d" buffer-len)))
	  (ov window-begin window-end 'face '(:background "#333333")))))

(defun cw/update-buffer ()
  (with-current-buffer cw/buffer
	(erase-buffer)
	(-each cw/windows (lambda (item) (with-current-buffer (cw/window-buffer item) (ov-clear))))
	(-each cw/windows
	  (lambda (window)
		(cw/mark-window-in-owners-buffer window)
		(cw/render-window-in-window-buffer window)))))

(defun cw/sanitize-window (window)
  (let* ((window-buffer (cw/window-buffer window))
		 (buffer-len (with-current-buffer window-buffer (buffer-size))))
	(when (<= (cw/window-region-begin window) 0)
	  (setf (cw/window-region-begin window) 1))
	(when (<= (cw/window-region-end window) 0)
	  (setf (cw/window-region-end window) 1))
	(when (> (cw/window-region-begin window) (1+ buffer-len))
	  (setf (cw/window-region-begin window) (1+ buffer-len)))
	(when (> (cw/window-region-end window) (1+ buffer-len))
	  (setf (cw/window-region-end window) (1+ buffer-len)))))

(defun cw/update-buffer-hook (beginning end length-before-edit)
  (let ((windows-in-buffer-being-changed (-filter (lambda (window) (equal (cw/window-buffer window) (current-buffer))) cw/windows))
		(needs-update 'nil))
	(-each windows-in-buffer-being-changed
	  (lambda (window-being-changed)
		(when window-being-changed
		  (let* ((window-begin (cw/window-region-begin window-being-changed))
				 (window-end (cw/window-region-end window-being-changed))
				 (length-after-edit (- end beginning))
				 (length-diff (- length-after-edit length-before-edit)))
			;;(message (format "Window begin (%d) - end (%d)" window-begin window-end))
			;;(message (format "From (%d) to (%d) Len before (%d), Len after(%d), DIFF %d" beginning end length-before-edit length-after-edit length-diff))
			(setf needs-update 't)
			(cond

			 ((or (and (<= beginning window-begin)
					   (>= end window-begin))
				  (and (< beginning window-begin)
					   (= beginning end)
					   (> (+ beginning length-before-edit) window-begin)))
			  (setf (cw/window-region-begin window-being-changed) beginning)
			  (setf (cw/window-region-end window-being-changed) (+ window-end length-diff)))

			 ((and (< end window-begin)
				   (< beginning window-begin))
			  (setf (cw/window-region-begin window-being-changed) (+ window-begin length-diff))
			  (setf (cw/window-region-end window-being-changed) (+ window-end length-diff)))

			 ((and (>= beginning window-begin)
				   (<= end (1+ window-end)))
			  (setf (cw/window-region-end window-being-changed) (+ window-end length-diff)))))
		  (cw/sanitize-window window-being-changed))))
	(when needs-update
	  (cw/update-buffer))))

(defun cw/create-code-window-buffer ()
  (unless cw/buffer
	(setq cw/buffer (get-buffer-create cw/code-window-buffer-name))
	(add-hook 'after-change-functions #'cw/update-buffer-hook)))

(defun cw/clear-windows ()
  (interactive)
  (setq cw/windows '())
  (cw/update-buffer))

(defun cw/regions-overlaps-p (from1 to1 from2 to2)
  (let ((minFrom (min from1 from2))
		(maxFrom (max from1 from2))
		(minTo (min to1 to2))
		(maxTo (max to1 to2)))
	(> minTo maxFrom)))


(defun cw/overlapping-window-exists (from to buffer)
  (let ((from-to-equal (= from to))
		(overlapping-window-exists (-any
									(lambda (window)
									  (and (equal (cw/window-buffer window) (current-buffer))
										   (cw/regions-overlaps-p from to (cw/window-region-begin window) (cw/window-region-end window))))
									cw/windows)))
	(message (format "From-to-equal %s" from-to-equal))
	(message (format "Overlapping w %s" overlapping-window-exists))
	(and (not from-to-equal)
		 overlapping-window-exists)))

(defun cw/create-window ()
  (interactive)
  (cw/create-code-window-buffer)

  (unless (equal (current-buffer) cw/buffer)
	(when (region-active-p)
	  (let ((from (region-beginning))
			(to (region-end)))
		(if (not (cw/overlapping-window-exists from to (current-buffer)))
			(progn
			  (add-to-list 'cw/windows (make-cw/window :buffer (current-buffer) :region-begin from :region-end to :name (buffer-name)))
			  (cw/update-buffer))
		  (message "A window already exists here. Delete it first if you want to create a new one"))))))
