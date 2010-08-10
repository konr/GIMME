;; Shamelessly copied from lunar-mode-line.el

(require 'timer)

(defgroup gimme-mode-line nil
  "Display GIMME information in mode line of Emacs."
  :group 'modeline)

(defvar gimme-status-mode-string nil
  "String to display in the mode line.")

(defvar gimme-status-mode-formats
  '((format "-[%s/%s]" time max)
    (format "-[%s]" time)
    (format "-[%d%%]" (/ (* 100 time-raw) max-raw)))
  "Not customizable due to free variables")

(defcustom gimme-status-mode-interval 1
  "*Seconds between updates of the mode line."
  :type 'integer
  :group 'gimme-mode-line)

(defvar gimme-status-mode-timer nil
  "Interval timer object.")

(defvar gimme-status-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line 'mouse-1)
      `(lambda (e)
         (interactive "e")
         (gimme-status-mode-toggle-format)
         (gimme-status-mode-update-line)))
    (define-key map (vector 'mode-line 'mouse-3)
      `(lambda (e)
         (interactive "e")
         (gimme)))

    map))

(define-minor-mode gimme-status-mode
  "Toggle display of track information in the mode line.
With a numeric arg, enable this display if arg is positive.

The mode line will be updated automatically every `gimme-status-mode-interval' seconds"
  :global t :group 'gimme-mode-line
  (setq gimme-status-mode-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and gimme-status-mode-timer (cancel-timer gimme-status-mode-timer))
  (if (not gimme-status-mode)
      (setq global-mode-string
            (delq 'gimme-status-mode-string global-mode-string))
    (add-to-list 'global-mode-string 'gimme-status-mode-string t)
    (setq gimme-status-mode-timer (run-at-time nil gimme-status-mode-interval
                                               'gimme-status-mode-update-line))
    (gimme-status-mode-update-line)))




(defun gimme-status-mode-toggle-format ()
  (interactive)
  (setq gimme-status-mode-formats
        (append (cdr gimme-status-mode-formats)
                (list (car gimme-status-mode-formats)))))

(defun gimme-status-mode-update-line ()
  "Updates the mode line."
  (when gimme-playtime
    (let* ((time-raw (cdr (assoc 'time gimme-playtime)))
           (max-raw  (cdr (assoc 'max  gimme-playtime)))
           (time (format-seconds "%.2m:%.2s" (/ time-raw 1000)))
           (max  (format-seconds "%.2m:%.2s" (/ max-raw 1000)))
           (msg (eval (car gimme-status-mode-formats))))
      (setq gimme-status-mode-string
            ;; FIXME: Random data
            (propertize msg
                        'mouse-face 'mode-line-highlight
                        'local-map gimme-status-mode-keymap
                        'help-echo "mouse-1: toggle status format\nmouse-2: jump to playlist\n"))
      (put 'gimme-status-mode-string 'risky-local-variable t)
      (comment setq gimme-status-mode-string (gimme-make-button))


      (force-mode-line-update)))
  (sit-for 0))


(provide 'gimme-status-mode)
