;;; gimme-inspect.el --- GIMME Interesting Music on My Emacs

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

;; Here we have a type of buffer that supports editing and saving
;; hash-like information. This will be used to change XMMS2 options
;; and track attributes.

;;; Code

(defvar gimme-inspect-map
  (let ((map (gimme-make-basic-map)))
    (define-key map (kbd "RET") 'gimme-inspect-change-current-line-prompt)
    (define-key map (kbd "S-<return>") 'gimme-inspect-change-current-line-prompt-reusing)
    (define-key map (kbd "W") 'gimme-inspect-write)
    (define-key map (kbd ",") 'gimme-inspect-print-current-key)
    (define-key map (kbd ".") 'gimme-inspect-print-current-value)
    (define-key map (kbd "y") 'gimme-inspect-yank-current-value)
    (define-key map (kbd "N") 'gimme-inspect-new-line-prompt)
    (define-key map (kbd "TAB") 'gimme-inspect-next-line)
    (define-key map (kbd "<backtab>") 'gimme-inspect-prev-line)
    map)
  "Inspect mode's keymap")

(defun gimme-inspect-mode ()
  "Mode used to edit hash-like information"
  (interactive)
  (font-lock-mode t)
  (use-local-map gimme-inspect-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-inspect-mode
        mode-name gimme-inspect-buffer-name)
  (setq-local previous-function nil))

(defun gimme-inspect (plist top-message write-function help-function)
  "Edits the information contained in plist, and writes it back using the write-function. A top message will be provided to the users, too"
  (interactive)
  (let* ((strs (mapcar (lambda (x) (format "%s" x)) plist))
         (new-keys (loop for x = plist then (cddr x) while x collect (car x)))
         (max (loop for x =  strs then (cddr x) while x
                    maximizing (length (car x)) into i
                    maximizing (length (cadr x)) into j
                    and finally return (list i j)))
         (max (mapcar (lambda (x) (min x gimme-inspect-max-length)) max))
         (total (+ 7 (apply #'+ max))))
    (gimme-on-buffer
     gimme-inspect-buffer-name
     (setq-local help-function help-function)
     (setq-local keys (mapcar #'symbol-name new-keys))
     (setq-local write-function write-function)
     (setq-local max-key (car max)) (setq-local max-val (cadr max))
     (delete-region (point-min) (point-max))
     (insert (format "%s\n\n   ---\n\n" top-message))
     (setq-local table-first-line (line-at-pos))
     (loop for x = strs then (cddr x) while x doing
           (gimme-inspect-insert-row (car x) (cadr x) max-key max-val))
     (setq-local initial-data plist)
     (setq-local table-last-line (1- (line-at-pos)))
     (gimme-inspect-mode)
     (switch-to-buffer gimme-inspect-buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gimme-inspect-collect-alist ()
  "Collects back the key-val pairs present in the buffer."
  (save-excursion
    (let* ((beg (progn (sane-goto-line table-first-line) (beginning-of-line) (point)))
           (end (progn (sane-goto-line table-last-line) (end-of-line) (point)))
           (plist (loop for key in (range-to-plists beg end)
                        when key collect (plist-get it 'data))))
      (plist-to-pseudo-alist plist))))

(defun gimme-inspect-get-current-key ()
  "Returns the key of the current line"
  (when (gimme-inspect-on-table-p)
    (get-text-property (+ (line-beginning-position) 2) 'data)))


(defun gimme-inspect-get-current-value ()
  "Returns the value of the current line"
  (when (gimme-inspect-on-table-p)
    (get-text-property (- (line-end-position) 3) 'data)))

(defun gimme-inspect-insert-row (key val key-size val-size)
  "Appends a key-val pair to the buffer."
  (goto-char (point-max))
  (insert (format "| %s | %s |\n"
                  (propertize (string-expanded key key-size) 'data key)
                  (propertize (string-expanded val val-size) 'font-lock-face `(:foreground ,(color-for val)) 'data val))))

(defun gimme-inspect-on-table-p ()
  "Returns whether you are or not in a valid line of the table"
  (and (>= (line-at-pos) table-first-line) (<= (line-at-pos) table-last-line)))

(defun gimme-inspect-change-current-line (new)
  "Changes the value of the current line to another thing"
  (when (gimme-inspect-on-table-p)
    (unlocking-buffer
     (save-excursion
       (let* ((beg (line-beginning-position)) (end (line-end-position))
              (old (gimme-inspect-get-current-value))
              (string (split-string (buffer-substring-no-properties beg end) "|"))
              (beg (+ beg 3 (length (nth 1 string)))) (end (- (line-end-position) 2))
              (new-string (propertize (string-expanded new gimme-inspect-max-length)
                                      'font-lock-face `(:foreground ,(color-for new) :weight bold) 'data new)))
         (unless (string= old new)
           (delete-region beg end) (goto-char beg) (insert new-string)
           (gimme-inspect-adjust-table)))))))

(defun gimme-inspect-adjust-table ()
  "Realigns everything"
  (let* ((beg (progn (sane-goto-line table-first-line) (beginning-of-line) (point)))
         (end (progn (sane-goto-line table-last-line) (end-of-line) (point)))
         (maxes (split-string (buffer-substring beg end) "\n"))
         (all-props (mapcar (lambda (x) (text-properties-at (- (length x) 3) x)) maxes))
         (maxes (mapcar (lambda (x) (length (replace-regexp-in-string " *|$" "" x))) maxes))
         (max (apply #'max maxes)))
    (sane-goto-line table-first-line)
    (setq max-val max)
    (dotimes (i (length maxes))
      (let* ((beg (+ (line-beginning-position) (nth i maxes)))
             (props (nth i all-props)))
        (delete-region beg (line-end-position))
        (goto-char (line-end-position))
        (insert (apply #'propertize (format " %s|" (make-string (- max (nth i maxes) ) ? )) props))
        (next-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-inspect-new-line-prompt ()
  "Adds a new key-val pair to what there currently is"
  (interactive)
  (let ((key (completing-read "Key? " keys))
        (val (completing-read "Value? " nil)))
    (unlocking-buffer (gimme-inspect-insert-row key val max-key max-val))
    (setq-local keys (cons key keys))))

(defun gimme-inspect-next-line ()
  "Moves to the next line"
  (interactive)
  (let ((line (max (1- table-first-line) (line-at-pos))))
    (sane-goto-line (if (<= table-last-line line) table-first-line (1+ line)))))

(defun gimme-inspect-prev-line ()
  "Moves to the previous line"
  (interactive)
  (let* ((line (line-at-pos))
         (line (min (if (or (> line table-last-line) (< line table-first-line))
                        (1- table-first-line) line))))
    (sane-goto-line (if (>= table-first-line line) table-last-line (1- line)))))

(defun gimme-inspect-print-current-key ()
  "Displays some description of the current key"
  (interactive)
  (funcall help-function (gimme-inspect-get-current-key)))

(defun gimme-inspect-print-current-value ()
  "Displays the value of the current line"
  (interactive)
  (message (gimme-inspect-get-current-value)))

(defun gimme-inspect-yank-current-value ()
  "Yanks the value of the current line"
  (interactive)
  (let ((val (gimme-inspect-get-current-value)))
    (with-temp-buffer (insert val) (kill-ring-save (point-min) (point-max))
                      (message "Yanked: %s" val))))


(defun gimme-inspect-change-current-line-prompt-reusing ()
  "Changes the value of the current line to another thing, reusing the previous value given, if possible"
  (interactive)
  (gimme-inspect-change-current-line-prompt t))

(defun gimme-inspect-change-current-line-prompt (&optional reuse)
  "Changes the value of the current line to another thing"
  (interactive)
  (gimme-inspect-change-current-line (read-from-minibuffer "Change to/with? " (if reuse (gimme-inspect-get-current-value) ""))))

(defun gimme-inspect-write ()
  "Writes back the configuration using the function given to `gimme-inspect'"
  (interactive)
  (funcall write-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actually uses of the gimme-inspect ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-get-conf ()
  "Asks XMMS2 for its configuration options"
  (interactive)
  (gimme-send-message "(conf)\n"))

(defun gimme-get-track-conf ()
  "Asks XMMS2 for the attributes of the current track"
  (interactive)
  (let ((id (get-text-property (point) 'id)))
    (when id (gimme-send-message "(track_conf %d)\n" id))))

(defun gimme-print-conf (alist)
  "Called by the ruby process to inspect XMMS2's options"
  (let ((plist (flatten alist))
        (msg "These are the configuration options of XMMS2.")
        (function #'gimme-inspect-write-options-to-xmms2)
	(help-function #'gimme-help-get-property))
    (gimme-inspect plist msg function help-function)))

(defun gimme-track-conf (alist)
  "Called by the ruby process to inspect a track's attributes"
  (let ((plist (flatten alist))
        (msg (format "There are the attributes of the track."))
        (function #'gimme-inspect-write-track-options-to-xmms2)
	(help-function #'gimme-help-get-property))
    (gimme-inspect plist msg function help-function)))

(defun gimme-inspect-write-options-to-xmms2 ()
  "Function used to write back options to XMMS2"
  (interactive)
  (gimme-send-message "(conf_save %s)\n"  (hyg-prin1 (gimme-inspect-collect-alist))))

(defun gimme-inspect-write-track-options-to-xmms2 ()
  "Function used to write back track attrivutes to XMMS2"
  (interactive)
  (let* ((alist (gimme-inspect-collect-alist))
         (fancy-format (loop for pair in alist collecting
                             (list (intern (car pair)) (if (number-in-string-p (cadr pair)) (string-to-number (cadr pair)) (cadr pair))))))
    (message "Sending changes to XMMS2")
    (gimme-send-message "(update_tags %s)\n"  (hyg-prin1 fancy-format))))

(provide 'gimme-inspect)
;;; gimme-inspect.el ends here
