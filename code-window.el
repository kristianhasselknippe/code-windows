(require 'dash)
(require 'cl)

(setq cw/buffer 'nil)

(setq cw/code-window-buffer-name "code-window-buffer")

(defun cw/create-code-window-buffer ()
  (unless cw/buffer
	(setq cw/buffer (get-buffer-create cw/code-window-buffer-name))
	(add-hook 'after-change-functions #'cw/update-buffer-hook)))

(setq cw/windows '())

(cl-defstruct cw/window buffer region-begin region-end)

(defun cw/get-text-from-window (window)
  (with-current-buffer cw/buffer
	(insert-buffer-substring (cw/window-buffer window) (cw/window-region-begin window) (cw/window-region-end window))))

(defun cw/clear-windows ()
  (setq cw/windows '()))

(defun cw/update-buffer ()
  (with-current-buffer cw/buffer
	(erase-buffer)
	(-each cw/windows
	  (lambda (item)
		(cw/get-text-from-window item)))))

(defun cw/update-buffer-hook (beginning end length)
  (let ((window-being-changed
		 (-first (lambda (window) (equal (cw/window-buffer window) (current-buffer))) cw/windows)))
	(message "Some editing is happening")
	(unless window-being-changed
	  (let ((window-begin (cw/window-region-begin window-being-changed))
			(window-end (cw/window-region-end window-being-changed)))
		(cond
		 ;; if the region being changed starts before our window, we assume all the content coming after should push our window further down
		 ((< start window-begin) )

		 ;; if region being changed is inside our window, might need to extend it?
		 ((and (> start window-begin)
			   (< end window-end))
		  )

		 ;; Region being changed is partially inside and partially below our window. Might need to extend the window.
		 ((and (> start window-begin)
			   (> end window-end))
		  ))

		(cw/update-buffer)))))

(defun cw/create-window ()
  (interactive)
  (cw/create-code-window-buffer)
  (when (region-active-p)
	(add-to-list 'cw/windows (make-cw/window :buffer (current-buffer) :region-begin (region-beginning) :region-end (region-end)))
	(cw/update-buffer)))
