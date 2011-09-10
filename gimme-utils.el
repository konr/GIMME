;;; gimme-utils.el --- Utility functions used on GIMME

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

;; A bunch of functions used by GIMME that might useful for other
;; things and therefore do not have its prefix.

;;; Code

(require 'hexrgb)

(defun major-mode (buffer)
  "Find out what the buffer's major-mode is"
  (with-current-buffer buffer major-mode))

(defun dfs-map (fun tree)
  "Maps a tree using depth-first search"
  (loop for node in tree collecting
        (if (listp node) (dfs-map fun node) (funcall fun node))))

(defun decode-strings-in-tree (tree encoding)
  "Decode all strings that are in the current tree"
  (dfs-map (lambda (x) (if (stringp x) (decode-coding-string x encoding) x)) tree))

(defmacro unlocking-buffer (&rest body)
  "Macro that allows safer manipulation of a read-only buffer"
  `(progn (toggle-read-only -1)
          ,@body
          (toggle-read-only 1)))

(defmacro setq-local (key val)
  "Sets a local function. It's a macro to avoid using quotes too much"
  `(progn (make-local-variable ',key) (set ',key ,val)))

(defun plist-to-alist (p)
  "Turns an alist into a plist"
  (loop for x = p then (cddr x) while x
        collecting (cons (car x) (cadr x))))

(defun plist-to-pseudo-alist (p)
  "((foo bar)) instead of ((foo . bar)) because the sexp library won't work otherwise :("
  (loop for x = p then (cddr x) while x
        collecting (list (car x) (cadr x))))

(defun plist-subset (small big)
  "Returns non-nil if all key/vals in small are also in big"
  (let ((keys (loop for s = small then (cddr s) while s
                    collecting (car s))))
    (every (lambda (n) (equal (getf small n) (getf big n))) keys)))

(defun color-for (string)
  "Deterministic way of selecting a color for a string"
  (let* ((colors gimme-colors)
         (len (length colors))
         (hash (string-to-number (substring (md5 (if (stringp string) string "")) 0 6) 16)))
    (nth (mod hash len) colors)))

(defun flatten (l)
  "Flattens a sexp recursively."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defmacro comment (&rest rest)
  "For debugging purposes")

(defun range-of-region ()
  "Returns the range between the lines in which the point and the mark are."
  (if (use-region-p)
      (let* ((min (min (point) (mark)))
             (max (max (point) (mark)))
             (min (progn (goto-char min) (line-beginning-position)))
             (max (+ 1 (progn (goto-char max) (line-end-position)))))
        (list min max))
    (list (line-beginning-position) (1+ (line-end-position)))))

(defun range-to-plists (p1 p2)
  "Returns all plists between points p1 and p2"
  (let ((min (min p1 p2)) (max (max p1 p2)))
    (loop for point = min then (next-property-change point)
          while (and point (> max point))
          collecting (text-properties-at point))))

(defun insidep (sexp1 sexp2)
  "Checks out if sexp1 is inside sexp2 "
  (or (equal sexp1 sexp2)
      (when (sequencep sexp2)
        (remove-if #'nil (mapcar (lambda (n) (insidep sexp1 n)) sexp2)))))


(defun sublistp (l1 l2)
  "Is l1 a sublist of l2?"
  (when (and (listp l1) (listp l2))
    (loop for l = l1 then (cdr l)
          and L = l2 then (cdr L)
          while (and l L (equal (car l) (car L)))
          collecting (list l L) into mu
          finally return
          (cond ((and (not l)  L)      t)
                ((and l       (not L)) nil)
                ((and (not l) (not L)) t)
                ((and l       L)       nil)))))

(defun get-bounds-where (f)
  "Returns the bounds where (f point) returns non-nil cycling through text-properties"
  (loop for beg = (point-min) then end
        and end = (next-property-change (point-min))
        then (next-property-change (or end (point-min)))
        while beg
        and when (funcall f beg)
        collect (list beg (or end (point-max))) end))

(defmacro with-focused-file-on-dired-mode-as-%file (&rest body)
  "To be used on dired-mode"
  `(let ((%file ,(dired-filename-at-point)))
     ,@body))

(defun string-expanded (string size &optional right-aligned)
  "Expands a string with whitespace or hides extra characters with an ellipsis"
  (let* ((string (if (<= (length string) size) string
                   (format "%s..." (substring string 0 (- size 3)))))
         (spaces (make-string (- size (length string)) ? ))
         (left (if right-aligned spaces string))
         (right (if right-aligned string spaces)))
    (format "%s%s" left right)))

(defun transpose (list-of-lists)
  "((a b c) (d e f)) -> ((a d) (b e) (c f)). Amazing, huh?"
  (apply #'map 'list (lambda (&rest args) (apply #'list args)) list-of-lists))

(defun sane-goto-line (number)
  "Without setting the mark"
  (goto-char (point-min))
  (next-line (1- number)))

(defun number-in-string-p (string)
  "Returns non-nil if the string can be safely converted to a number."
  (string-match "^\\(\\([0-9]\+\.[0-9]\*\\)\\|\\([0-9]\+\\)\\|\\([0-9]*\.[0-9]\+\\)\\)$" string))

(defun alist-put (alist key val)
  "Sets or updates an alist"
  (loop for pair in alist
        unless (equal key (car pair)) collect pair into pairs
        finally return (cons (cons key val) pairs)))

(defun completing-read-with-whitespace (prompt options)
  "Jesus Christ..."
  (let ((prev (cdr (assoc 32 minibuffer-local-completion-map)))
        (placeholder (define-key completion-list-mode-map (kbd "SPC") 'self-insert-command))
        (data (completing-read prompt options)))
    (define-key completion-list-mode-map (kbd "SPC") prev)
    data))

(defun plist-get-with-equal (coll key &optional fun)
  "Allah the Merciful..."
  (loop for x = coll then (cddr x)
        and fun = (or fun 'equal)
        while x if (funcall fun key (car x)) return (cadr x)))

(defun visible-buffers ()
  "Returns a list of visible buffers"
  (mapcan #'buffer-list (visible-frame-list)))

(defun expand-directories (files)
  "Given a list of files and directories, expand the directories recursively, adding their children to list"
  (mapcan (lambda (x) (if (file-directory-p x) (expand-directories (directory-files x t))
                   (list x))) (remove-if (lambda (x) (string-match "/\\.\\.?$" x)) files)))

(defun hyg-prin1 (&rest args)
  "Saves the value of print-level and print-length and then runs prin1-to-string"
  (let ((level print-level) (length print-length))
    (setq print-level nil print-length nil)
    (let ((prin1 (apply #'prin1-to-string args)))
      (setq print-level level print-length length)
      prin1)))

(defun kill-current-buffer ()
  "Kills the current-buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defun decode-percent-encoding (string)
  "Makes an URL-formatted string human-readable."
  (replace-regexp-in-string "+" " " (decode-coding-string (url-unhex-string string) 'utf-8)))

(defun time-distance-to-now (unix-time)
  "Returns a string like '100 years ago'."
  (let* ((diff (- (string-to-int (format-time-string "%s")) unix-time))
         (from-ago (if (> diff 0) "ago" "in the future"))
         (diff (if (> diff 0) diff (- diff)))
         (interval-alist '(("second" . 60) ("minute" . 60) ("hour" . 24) ("day" . 30) ("month" . 12) ("year" . 1000))))
    (loop for intervals = interval-alist then (cdr intervals)
          and time = diff then (/ time (or (cdar intervals) 1))
          while (and intervals (<= (cdar intervals) time)) finally return
          (if intervals (format "%s %s%s %s." time (caar intervals) (if (> time 1) "s" "") from-ago)
            "A long time ago in a galaxy far, far away...."))))

(defun prettify-lyrics (lyrics &optional count)
  "Removes empty lines when it's a recurring pattern ATM"
  (let* ((empty (length (regexp-all-matches lyrics "^\n")))
         (all (length (regexp-all-matches lyrics "\n")))
         (count (or count 0)))
    (if (and (> 20 count) (> (* empty 4) all))
        (prettify-lyrics (replace-regexp-in-string "\n\n" "\n" lyrics) (1+ count)) lyrics)))

(provide 'gimme-utils)
;;; gimme-utils.el ends here
