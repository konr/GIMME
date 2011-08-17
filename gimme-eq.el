(defvar gimme-eq-time 1)
(defvar gimme-eq-div 2)
(defvar gimme-eq-keys 7)
(defconst gimme-eq-chans 31)

(defvar gimme-presets
  ;; FIXME: tbd
  '("1965" (-20 -16 -7 -4 -4 -4 -7 -7 3 3 -2 -4 4 1 1 -4 -6 -12)
    "Air" (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 3 2)
    "Brittle" (-12 -10 -9 -8 -7 -6 -5 -3 -2 -2 -2 -2 -1 1 4 4 1 0)
    "Car Stereo" (-5 0 1 0 0 -4 -4 -5 -5 -5 -3 -2 -2 0 1 0 -2 -5)
    "Classic V" (5 2 0 -2 -5 -6 -8 -8 -7 -7 -4 -3 -1 1 3 5 5 4)
    "Clear" (1 1 0 0 0 -3 0 0 0 0 0 0 0 0 2 2 2 1)
    "Dark" (-6 -2 -2 -2 -2 -2 -2 -2 -2 -2 -2 -5 -8 -10 -12 -14 -18 -18)
    "DEATH" (20 17 12 8 4 0 0 0 0 0 0 0 0 0 0 0 0 0)
    "Drums" (2 1 0 0 0 -2 0 -2 0 0 0 0 2 0 0 3 0 0)
    "Flat" (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    "Home Theater" (5 2 0 -2 -3 -5 -6 -6 -5 -2 -1 0 -1 -3 3 4 3 0)
    "Loudness" (4 4 4 2 -2 -2 -2 -2 -2 -2 -2 -4 -10 -7 0 3 4 4)
    "Metal" (4 5 5 3 0 -1 -2 -1 0 1 1 1 1 0 -1 -1 -1 -1)
    "Pop" (6 5 3 0 -2 -4 -4 -6 -3 1 0 0 2 1 2 4 5 6)
    "Premaster" (0 1 3 0 -3 -3 0 0 0 2 0 0 3 0 3 1 3 2)
    "Presence" (0 0 0 0 0 0 0 0 0 3 5 4 3 2 0 0 0 0)
    "Punch & Sparkle" (3 5 3 -1 -3 -5 -5 -3 -2 1 1 1 0 2 1 3 5 3)
    "Shimmer" (0 0 0 -2 -2 -7 -5 0 0 0 0 0 4 1 3 3 4 0)
    "Soft Bass" (3 5 4 0 -13 -7 -5 -5 -1 2 5 1 -1 -1 -2 -7 -9 -14)
    "Strings" (-3 -4 -4 -5 -5 -4 -4 -3 -2 -2 -2 -2 -1 2 3 0 -2 -2)))

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
