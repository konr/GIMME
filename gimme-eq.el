(defvar gimme-eq-time 1)
(defvar gimme-eq-div 2)
(defvar gimme-eq-keys 7)
(defconst gimme-eq-chans 31)

(defun gimme-eq-string (eq)
  (let* ((string (apply #'concat (mapcar #'gimme-eq-number-to-bar eq))))
    (format "[%s]"(propertize string 'font-lock-face '(:foreground "#ff0000")))))

(defun gimme-eq-show ()
  (let* ((string (gimme-eq-string)) (time (format "%d seconds" gimme-eq-time)))
    (setq gimme-status-mode-formats (cons string gimme-status-mode-formats))
    (run-at-time gimme-eq-time nil (lambda () (setq gimme-status-mode-formats
                                                    (remove-if (lambda (x) (equal x (gimme-eq-string)))
                                                               gimme-status-mode-formats))))))

(defun gimme-eq-change (beg end change) (gimme-send-message "(eqchange %d %d %d)\n" beg end change))

(defun gimme-eq-print (values)
  (let* ((s (gimme-eq-string values)) (time (format "%d seconds" gimme-eq-time))
         (function (lambda (s) (setq gimme-status-mode-formats (remove-if (lambda (x) (equal x s)) gimme-status-mode-formats)))))
    (setq gimme-status-mode-formats (cons s gimme-status-mode-formats))
    (run-at-time time nil function s)))

(defun gimme-eq-number-to-bar (number)
  (if (or (> number 20) (< number -20)) "☃"
    (getf '(0 " " 1 "▁" 2 "▂" 3 "▃" 4 "▄" 5 "▅" 6 "▆" 7 "▇" 8 "█")
          (floor (/ (+ 20 number) 5)))))

(defun gimme-eq-draw ()
  (interactive)
  (let* ((given (read-from-minibuffer "Use digits in [0-9]: "))
         (adjusted (mapcar (lambda (x) (/ (* 20 (- (string-to-int x) 4.5)) 4.5)) (cdr (butlast (split-string given "")))))
         (step (/ (1- (length adjusted)) 30.0))
         (adjusted (loop for i from 0 upto 30 collecting
                         (let* ((pos (* i step)) (fl (truncate pos)) (perc (- pos fl))
                                (weight (+ (* (- 1 perc) (nth fl adjusted)) (* perc (or (nth (1+ fl) adjusted) (nth fl adjusted))))))
                           weight)))
         (alist (loop for el in adjusted and i = 0 then (1+ i)
                      collecting (list (format "equalizer.gain%02d" i) (format "%f" el)))))
    (if (string-match "^[0-9]\+$" given)
        (progn (gimme-send-message "(conf_save %s nil)\n"(prin1-to-string alist))
	       (gimme-send-message "(eqgain)\n"))
      (message "Invalid input!"))))

(provide 'gimme-eq)
