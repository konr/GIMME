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

;; Implements a feature-rich tagwriter with scanning and all.

;;; Code

(defun gimme-tagwriter-mode ()
  "Mode used to change tags in a list of tracks."
  (interactive)
  (font-lock-mode t)
  (use-local-map gimme-tagwriter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-tagwriter-mode
        mode-name gimme-tagwriter-buffer-name)
  (setq-local previous-function nil)
  (setq-local previous-pattern nil)
  (add-hook 'minibuffer-exit-hook #'gimme-tagwriter-undo-previews nil t))

(defvar gimme-tagwriter-map
  (let ((map (gimme-make-basic-map)))
    (define-key map (kbd "y") 'gimme-tagwriter-yank-current-field)
    (define-key map (kbd "RET") 'gimme-tagwriter-apply-function)
    (define-key map (kbd "S-<return>") 'gimme-tagwriter-apply-previous-function)
    (define-key map (kbd "C-S-<return>")
      'gimme-tagwriter-apply-previous-function-to-all-songs)
    (define-key map (kbd "W") 'gimme-tagwriter-write-to-mlib)
    (define-key map (kbd ".") 'gimme-tagwriter-print-current-field)
    (define-key map (kbd "TAB") 'gimme-tagwriter-next-field)
    (define-key map (kbd "s") 'gimme-tagwriter-scan-current)
    (define-key map (kbd "S") 'gimme-tagwriter-scan-current-reusing)
    (define-key map (kbd "!") 'gimme-tagwriter-scan-all)
    (define-key map (kbd "<backtab>") 'gimme-tagwriter-prev-field)
    map)
  "Tagwriter's keymap")

(defvar gimme-tagwriter-filter-map
  (let ((map (copy-tree minibuffer-local-map)))
    (define-key map (kbd "TAB") 'gimme-tagwriter-recalculate-tags)
    map)
  "Tagwriter's filter keymap")


(defvar gimme-tagwriter-scan-map
  (let ((map minibuffer-local-map))
    (define-key map (kbd "TAB") 'gimme-tagwriter-recalculate-tags)
    map)
  "Tagwriter's scan keymap")

(defun gimme-tagwriter ()
  "A feature-rich tagwriter with scanning and all."
  (interactive)
  (let* ((old-and-fixed (gimme-tagwriter-fix-data (apply #'range-to-plists (range-of-region))))
         (fixed (cadr old-and-fixed)))
    (gimme-on-buffer
     gimme-tagwriter-buffer-name
     (delete-region (point-min) (point-max))
     (let* ((colls (length (car fixed)))
            (rows (length fixed))
            (max-list (mapcar (lambda (x) (min gimme-tagwriter-max-length (apply #'max x))) (transpose (mapcar (lambda (x) (mapcar #'length x)) fixed)))))
       (setq-local data (car old-and-fixed))
       (setq-local mass-operation nil)
       (setq-local colls colls) (setq-local rows rows)
       (loop for row in fixed
             doing (insert "| ")
             doing (loop for coll in row and i = 0 then (1+ i) doing (insert (format "%s | " (string-expanded coll (nth i max-list)))))
             doing (insert "\n"))
     (gimme-tagwriter-mode))))
  (switch-to-buffer gimme-tagwriter-buffer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tagwriter-set-cell (row coll val)
  "Sets the value of a cell."
  (save-excursion
    (unlocking-buffer
     (let* ((bounds (gimme-tagwriter-cell-boundaries row coll))
            (beg (car bounds)) (end (cadr bounds)))
       (delete-region beg end) (goto-char beg)
       (insert (format " %s " val))
       ;; FIXME: This is the place that must separated.
       (let ((largest (loop for r from 0 upto (1- rows) collecting
                            (progn (goto-char (cadr (gimme-tagwriter-cell-boundaries r coll)))
                                   (current-column))
                            into n and finally return (apply #'max n))))
         (loop for r from 0 upto (1- rows)
               doing (let ((end (cadr (gimme-tagwriter-cell-boundaries r coll))))
                       (goto-char end)
                       (dotimes (i (- largest (current-column))) (insert " ")))))))))

(defun gimme-tagwriter-get-cell (row coll)
  "Gets the value of a cell."
  (save-excursion
    (goto-char (1+ (car (gimme-tagwriter-cell-boundaries row coll))))
    (text-properties-at (point))))

(defun gimme-tagwriter-cell-boundaries (row coll)
  "Assuming the entire document is a table"
  (sane-goto-line (1+ row))
  (beginning-of-line)
  (if (equal coll 0)
      (list (1+ (point)) (1- (car (gimme-tagwriter-cell-boundaries row 1))))
    (loop for i from 0 upto coll
          collecting (progn (forward-char) (find-char-forward ?|) (point)) into all
          finally return (list (+ 2 (car (last (butlast all)))) (+ 1 (car (last all)))))))

(defun find-char-forward (char)
  "Finds a character... forward"
  (unless (equal (char-after (1+ (point))) char)
    (forward-char)
    (find-char-forward char)))


(defun gimme-tagwriter-current-cell ()
  "Gets the current cell"
  (list (max 0 (1- (line-number-at-pos)))
        (max 0 (1+ (- colls (length (split-string (substring (buffer-substring-no-properties (line-beginning-position) (line-end-position)) (current-column)) "|")))))))

(defun gimme-tagwriter-fix-data (data)
  "Turns a list of plists, in the format used in gimme-playlist, into a table-like structure."
  (let* ((relevant (loop for j in data and line = 1 then (1+ line) collecting
                         (loop for i = j then (cddr i) while i
                               if (not (member (car i) '(type name timesplayed face duration id font-lock-face pos)))
                               collect (list (car i) (format "%s" (cadr i))) into pairs end
                               finally return (mapcan (lambda (x) x) pairs))))
         (keys (loop for i = (car relevant) then (cddr i) while i collecting
                     (propertize (symbol-name (car i)) 'font-lock-face `(:foreground ,(color-for (symbol-name (car i)))
                                                                                     :weight bold))))
         (vals (loop for j in relevant collecting
                     (loop for tag in keys collecting
                           (let* ((tag (intern tag)) (text (plist-get j tag)))
                             (propertize
                              (if (>= (length text) gimme-tagwriter-max-length)
                                  (format "%s..." (substring text 0 (- gimme-tagwriter-max-length 3))) text)
                              'font-lock-face `(:foreground ,(color-for (symbol-name tag))) 'type tag 'val text)
                             ))
                     )))
    (list data (cons keys vals))))

(defun gimme-tagwriter-eval-formula (formula)
  "Evals a given formula, replacing the variables and such."
  (let* ((plist (range-to-plists (line-beginning-position) (line-end-position)))
         (plist (loop for item in plist if item
                      collect (list (plist-get item 'type) (plist-get item 'val))
                      into alist end finally return (mapcan (lambda (x) x) alist)))
         (text (replace-regexp-in-string
                "$[^ ]\+"
                (lambda (x) (format "%s" (plist-get plist (intern (substring x 1)))))
                formula)))
    text))

(defun regexp-all-matches (string regexp)
  "All matches of a regexp, given a string."
  (with-temp-buffer
    (insert string) (goto-char (point-min))
    (loop for x = (ignore-errors (search-forward-regexp regexp))
          then (ignore-errors (search-forward-regexp regexp))
          and y = -1 then x while (and x (not (= 0 (- x y)))) collecting
          (loop for i = 1 then (1+ i) while (match-string i)
                collecting (match-string i)))))

(defun gimme-tagwriter-scan (string regexp)
  "Scans all fields, given the regexp"
  (let* ((new-regexp (replace-regexp-in-string "$[a-zA-Z]\+" "\\\\\(\.\*\\\\\)"
                                               regexp))
         (symbols (mapcar (lambda (x) (intern (substring (car x) 1)))
                          (regexp-all-matches regexp "\\($[a-zA-Z]\+\\)")))
         (matches (car (regexp-all-matches string new-regexp))))
    (loop for i = 0 then (1+ i) while (nth i matches)
          collecting (list (nth i symbols) (nth i matches)) into alist
          and finally return (mapcan (lambda (x) x) alist))))

(defun gimme-tagwriter-get-field (line-number field)
  "Gets a field from the current line by the name"
  (plist-get (gimme-tagwriter-get-vals line-number) field))

(defun gimme-tagwriter-get-vals (line-number)
  "Gets all fields and their names from the current line"
  (let* ((range (save-excursion (sane-goto-line line-number)
                                (list (line-beginning-position) (line-end-position))))
         (line (apply #'range-to-plists range))
         (plist (loop for f in line if f
                      collect (list (plist-get f 'type) (plist-get f 'val))
                      into alist and finally return (mapcan (lambda (x) x) alist))))
    plist))

(defun gimme-tagwriter-undo-previews ()
  "FIXME: Not working"
  (with-current-buffer gimme-tagwriter-buffer-name
    (undo undos) (setq-local undos 0)))


(defun gimme-tagwriter-set-vals (line-number plist)
  "Set a bunch of vals in a given line"
  (let* ((range (save-excursion (sane-goto-line line-number)
                                (list (line-beginning-position) (line-end-position))))
         (tags (loop for f in (apply #'range-to-plists range)
                     if f collect (plist-get f 'type))))
    (loop for tag in tags and i = 0 then (1+ i) if (member tag plist)
          do (progn (gimme-tagwriter-update-cell line-number i (getf plist tag))
                    (setq-local undos (1+ undos))))))

(defun gimme-tagwriter-update-cell (line coll val)
  "Sets a new value for a cell"
  (let* ((old (gimme-tagwriter-get-cell (1- line) coll))
         (new-plist (plist-put old 'val val)) (max gimme-tagwriter-max-length))
    (gimme-tagwriter-set-cell
     (1- line) coll
     (apply #'propertize
            (if (>= (length val) max)
                (format "%s..." (substring val 0 (- max 3))) val) new-plist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gimme-tagwriter-print-current-field ()
  "Prints current field."
  (interactive)
  (let* ((type (get-text-property (point) 'type))
         (val (get-text-property (point) 'val))
         (val (if (equal type 'url) (decode-percent-encoding val) val)))
    (when val (message (replace-regexp-in-string "%" "%%" val)))))

(defun gimme-tagwriter-yank-current-field ()
  "Yanks current field."
  (interactive)
  (let ((val (get-text-property (point) 'val)))
    (with-temp-buffer (insert val) (kill-ring-save (point-min) (point-max))
                      (message "Yanked: %s" val))))

(defun gimme-tagwriter-next-field ()
  "Goes to the next field"
  (interactive)
  (let* ((cur (gimme-tagwriter-current-cell))
         (next (list (car cur) (1+ (cadr cur))))
         (next (if (>= (cadr next) colls) (list (1+ (car cur)) 0) next))
         (next (if (>= (car next) rows) (list 1 0) next)))
    (goto-char (1+ (car (apply #'gimme-tagwriter-cell-boundaries next))))
    (gimme-tagwriter-print-current-field)))

(defun gimme-tagwriter-prev-field ()
  "Goes to the previous field"
  (interactive)
  (let* ((cur (gimme-tagwriter-current-cell))
         (prev (list (car cur) (1- (cadr cur))))
         (prev (if (< (cadr prev) 0) (list (1- (car cur)) (1- colls)) prev))
         (prev (if (< (car prev) 1) (list (1- rows) (1- colls)) prev)))
    (goto-char (1+ (car (apply #'gimme-tagwriter-cell-boundaries prev))))
    (gimme-tagwriter-print-current-field)))

(defun gimme-tagwriter-recalculate-tags ()
  "Recalculates the tags, given the pattern in the minibuffer."
  (interactive)
  (let* ((regexp (minibuffer-contents))
         (url (with-current-buffer gimme-tagwriter-buffer-name url))
         (plist (gimme-tagwriter-scan url regexp)))
    (with-current-buffer gimme-tagwriter-buffer-name
      (unlocking-buffer
       (comment gimme-tagwriter-undo-previews)
       (gimme-tagwriter-set-vals line plist)))))


(defun gimme-tagwriter-write-to-mlib ()
  "Write changes back to the Mlib"
  (interactive)
  (when (y-or-n-p "Write tags to the Medialib? ")
    (loop for datum in data and line = 1 then (1+ line) collecting
          (loop for head = (gimme-tagwriter-get-vals (1+ line)) then (cddr head)
                and new-plist = (plist-put datum (car head) (cadr head))
                then (plist-put datum (car head) (cadr head))
                while head finally return (butlast (butlast new-plist)))
          into plists and finally (dolist (plist plists) 
				    (gimme-send-message "(update_tags %s)\n" (hyg-prin1 (plist-to-pseudo-alist plist)))))))

(defun gimme-tagwriter-scan-current ()
  "Scans fields from the URL, using, if possible, the previous formula."
  (interactive)
  (gimme-tagwriter-scan-current t))

(defun gimme-tagwriter-scan-current (&optional try-previous)
  "Scans fields from the URL."
  (interactive)
  (unlocking-buffer
   (let* ((line (line-number-at-pos)) (fields (gimme-tagwriter-get-vals line))
          (available (loop for head = fields then (cddr head) while head
                           collecting (format "$%s" (car head))))
          (url (decode-percent-encoding (plist-get fields 'url)))
          (max gimme-tagwriter-max-length))
     (setq-local url url)
     (setq-local line line)
     (setq-local undos 0)
     (let* ((pattern (if (and try-previous previous-pattern)
                         previous-pattern (read-from-minibuffer (format "%s> " url) "" gimme-tagwriter-scan-map)))
            (plist (gimme-tagwriter-scan url pattern)))
       (setq-local previous-pattern pattern)
       (gimme-tagwriter-set-vals line plist)))))


(defun gimme-tagwriter-apply-previous-function ()
  "Apply previous change to the current cell"
  (interactive)
  (gimme-tagwriter-apply-function previous-function))

(defun gimme-tagwriter-apply-function (&optional raw field)
  "Apply a function to or sets the current cell"
  (interactive)
  (let* ((raw (or raw (read-from-minibuffer "Change to/with? " ""
                                            gimme-tagwriter-filter-map)))
         (function (read raw))
         (current-field (or field (gimme-tagwriter-current-cell)))
         (data (plist-get (apply #'gimme-tagwriter-get-cell current-field) 'val))
         (current-field (list (1+ (car current-field)) (cadr current-field)))
         (processed (if (functionp function) (funcall function data) raw)))
    (apply #'gimme-tagwriter-update-cell `(,@current-field ,processed))
    (setq-local previous-function raw)))

(defun gimme-tagwriter-apply-previous-function-to-all-songs ()
  "Applies previous change to the same field of all songs."
  (interactive)
  (let ((coll (cadr (gimme-tagwriter-current-cell))))
    (loop for line from 1 to (1- rows) doing
          (gimme-tagwriter-apply-function previous-function (list line coll)))))

(defun gimme-tagwriter-scan-all ()
  "Scans fields from the URL of all songs."
  (interactive)
  (setq-local mass-operation t)
  (save-excursion
    (goto-char (point-min))
    (loop for i upto (1- rows) doing (next-line)
          and doing (gimme-tagwriter-scan-current t)))
  (setq-local mass-operation nil))
(provide 'gimme-tagwriter)


;;; gimme-tagwriter.el ends here
