;; Version: $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prolusion/toggle-overlay (&optional arg verbose)
  ""
  (interactive (list current-prefix-arg t))
  (if (or (null arg)
	  (if (> (prefix-numeric-value arg) 0)
	      (not follow-mouse)
	    follow-mouse))
      ;; Toggle it:
      (progn
	(cond ((setq follow-mouse (not follow-mouse))
	       ;; Save the current value of track-mouse before (re)setting it:
	       (put 'follow-mouse 'track-mouse track-mouse)
	       (setq track-mouse t)
	       ;; Save the current binding of [mouse-movement] before
	       ;; (re)binding it:
	       (put 'follow-mouse 'mouse-movement
		    (lookup-key special-event-map [mouse-movement]))
	       (define-key special-event-map [mouse-movement]
		 'follow-mouse-select-window))
	      (t			; disable
	       ;; Restore the previous value of track-mouse:
	       (setq track-mouse (get 'follow-mouse 'track-mouse))
	       ;; Restore the previous binding of [mouse-movement]:
	       (define-key special-event-map [mouse-movement]
		 (get 'follow-mouse 'mouse-movement))))
	(if (or (interactive-p) verbose)
	    (message "Follow mouse is %s"
		     (if follow-mouse "enabled" "disabled"))))
    (if (or (interactive-p) verbose)
	(message "Follow mouse is already %s"
		 (if follow-mouse "enabled" "disabled"))))
  ;; Return the result:
  follow-mouse)

(defun follow-mouse-select-window (event)
  "*Like `mouse-select-window', if `follow-mouse' is set.
Otherwise, do nothing; in particular, don't generate an error if EVENT
occurs outside a window or in an inactive minibuffer window.
See `follow-mouse-deselect-active-minibuffer' and
`follow-mouse-auto-raise-frame'."
  (interactive "e")
  (prog1 (if follow-mouse
	     (let ((current-window (get-buffer-window (current-buffer)))
		   (event-window (posn-window (event-start event))))
	       (if (and (or (not (window-minibuffer-p current-window))
			    (not (minibuffer-window-active-p current-window))
			    follow-mouse-deselect-active-minibuffer)
			(windowp event-window)
			(or (not (window-minibuffer-p event-window))
			    (minibuffer-window-active-p event-window)))
		   (progn
		     (or (eq (window-buffer current-window)
			     (window-buffer event-window))
			 (run-hooks 'mouse-leave-buffer-hook))
		     (if follow-mouse-auto-raise-frame
			 (mouse-select-window event)
		       (select-window event-window))))))
    ;; Enable dragging:
    (setq unread-command-events
	  (nconc unread-command-events (list event)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'prolusion-overlay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prolusion-overlay.el ends here
