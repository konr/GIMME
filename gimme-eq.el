;;; gimme-eq.el --- GIMME Interesting Music on My Emacs

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: XMMS2, mp3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; A couple of functions that implement equalizer control

;;; Code

(defconst gimme-eq-chans 31
  "Channels used by XMMS2. Let's be fancy and take them all.")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-eq-string (eq)
  "Generates a string representing the status of the equalizer."
  (let* ((string (apply #'concat (mapcar #'gimme-eq-number-to-bar eq))))
    (format "[%s]"(propertize string 'font-lock-face '(:foreground "#ff0000")))))

(defun gimme-eq-print (values)
  "Called by the Ruby process, displays the equalizer."
  (let* ((s (gimme-eq-string values)) (time (format "%d seconds" gimme-eq-time))
         (function (lambda (s) (setq gimme-status-mode-formats (remove-if (lambda (x) (equal x s)) gimme-status-mode-formats)))))
    (setq gimme-status-mode-formats (cons s gimme-status-mode-formats))
    (run-at-time time nil function s)))

(defun gimme-eq-number-to-bar (number)
  "Maps a number, from -20 to 20, to a bar"
  (if (or (> number 20) (< number -20)) "☃"
    (getf '(0 " " 1 "▁" 2 "▂" 3 "▃" 4 "▄" 5 "▅" 6 "▆" 7 "▇" 8 "█")
          (floor (/ (+ 20 number) 5)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gimme-eq-change (beg end change) 
  "Increases the value of the channels in range [beg;end] by the value given."
  (gimme-send-message "(eqchange %d %d %d)\n" beg end change))

(defun gimme-eq-draw ()
  "\"Draws\" the desirable equalizer using as many digits in [0-9] as desired."
  (interactive)
  (let* ((given (read-from-minibuffer "Use as many digits in [0-9] as you'd like: "))
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
;;; gimme-eq.el ends here
