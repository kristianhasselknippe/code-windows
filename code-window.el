(require 'dash)
(require 'cl)

(setq cw/buffer 'nil)

(setq cw/code-window-buffer-name "code-window-buffer")

(setq cw/windows '())

(cl-defstruct cw/window buffer region-begin region-end)

(defun cw/get-text-from-window (window)
  (with-current-buffer cw/buffer
	(insert-buffer-substring (cw/window-buffer window) (cw/window-region-begin window) (cw/window-region-end window))))

(defun cw/clear-windows ()
  (setq cw/windows '()))

(defun cw/update-buffer ()
  (message "Updating buffer")
  (with-current-buffer cw/buffer
	(erase-buffer)
	(-each cw/windows
	  (lambda (item)
		(cw/get-text-from-window item)))))

(defun cw/update-buffer-hook (beginning end length-before-edit)
  (let ((window-being-changed
		 (-first (lambda (window) (equal (cw/window-buffer window) (current-buffer))) cw/windows)))
	(when window-being-changed
	  (let* ((window-begin (cw/window-region-begin window-being-changed))
			(window-end (cw/window-region-end window-being-changed))
			(length-after-edit (- window-end window-begin))
			(length-diff (- length-after-edit length-before-edit)))
		(cond
		 ((< end window-begin)
		  (message "Change was above and inside the window")
		  (setf (cw/window-region-begin window-being-changed) (+ window-begin length-diff)))
		 ((and (> start window-start)
			   (< end window-end))
		  (message "Change was inside window")
		  (setf (cw/window-region-begin window-being-changed) (+ window-begin length-diff))))

		(cw/update-buffer)))))

(defun cw/create-code-window-buffer ()
  (unless cw/buffer
	(setq cw/buffer (get-buffer-create cw/code-window-buffer-name))
	(add-hook 'after-change-functions #'cw/update-buffer-hook)))

(defun cw/create-window ()
  (interactive)
  (cw/create-code-window-buffer)
  (when (region-active-p)
	(add-to-list 'cw/windows (make-cw/window :buffer (current-buffer) :region-begin (region-beginning) :region-end (region-end)))
	(cw/update-buffer)))
