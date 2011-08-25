;;; gimme-status-mode.el --- GIMME's modeline utility

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

;; Shamelessly copied from lunar-mode-line.el

;;; Code


(require 'timer)

(defvar gimme-status-mode-string nil
  "String to display in the mode line.")

(defvar gimme-status-mode-formats
  '((format "-[%s/%s]" time max)
    (format "-[%s]" time)
    (format "-[%d%%]" (/ (* 100 time-raw) max-raw)))
  "Not customizable due to free variables")

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
    map)
  "Actually a 'mousemap'")

(define-minor-mode gimme-status-mode
  "Toggle display of track information in the mode line.
With a numeric arg, enable this display if arg is positive.

The mode line will be updated automatically every second"
  :global t :group 'gimme-mode-line
  (setq gimme-status-mode-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and gimme-status-mode-timer (cancel-timer gimme-status-mode-timer))
  (if (not gimme-status-mode)
      (setq global-mode-string
            (delq 'gimme-status-mode-string global-mode-string))
    (add-to-list 'global-mode-string 'gimme-status-mode-string t)
    (setq gimme-status-mode-timer (run-at-time nil 1 'gimme-status-mode-update-line))
    (gimme-status-mode-update-line)))




(defun gimme-status-mode-toggle-format ()
  "Toggles between the various formats of displaying the current position"
  (interactive)
  (setq gimme-status-mode-formats
        (append (cdr gimme-status-mode-formats)
                (list (car gimme-status-mode-formats)))))

(defun gimme-status-mode-update-line ()
  "Updates the modeline."
  (when gimme-playtime
    (let* ((time-raw (cdr (assoc 'time gimme-playtime)))
           (max-raw  (cdr (assoc 'max  gimme-playtime)))
           (time (format-seconds "%.2m:%.2s" (/ time-raw 1000)))
           (max  (format-seconds "%.2m:%.2s" (/ max-raw 1000)))
           (msg (eval (car gimme-status-mode-formats))))
      (setq gimme-status-mode-string
            (propertize msg
                        'mouse-face 'mode-line-highlight
                        'local-map gimme-status-mode-keymap
                        'help-echo "mouse-1: toggle status format\nmouse-2: jump to playlist\n"))
      (put 'gimme-status-mode-string 'risky-local-variable t)
      (comment setq gimme-status-mode-string (gimme-make-button))


      (force-mode-line-update)))
  (sit-for 0))


(provide 'gimme-status-mode)
;;; gimme-status-mode.el ends here
