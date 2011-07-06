;;; gimme-utils.el --- Utility functions used on GIMME

;; Author: Konrad Scorciapino <scorciapino@gmail.com>
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

;;; Code

(defun major-mode (buffer)
  "Find out what the buffer's major-mode is"
  (with-current-buffer buffer major-mode))

(defun dfs-map (fun tree)
  (loop for node in tree collecting
        (if (listp node) (dfs-map fun node) (funcall fun node))))

(defun decode-strings-in-tree (tree encoding)
  (dfs-map (lambda (x) (if (stringp x) (decode-coding-string x encoding) x)) tree))

(defmacro unlocking-buffer (&rest body)
  `(progn (toggle-read-only -1)
          ,@body
          (toggle-read-only 1)))

(defun plist-to-alist (p)
  (loop for x = p then (cddr x) while x
        collecting (cons (car x) (cadr x))))

(defun plist-to-pseudo-alist (p)
  "((foo bar)) instead of ((foo . bar)). The sexp library won't work otherwise :("
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
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defmacro comment (&rest rest)
  "For debugging purposes")

(defun range-of-region ()
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

(defun appropriate-colors (&optional step)
  "Returns all colors that, according to w3c, will be readable on your background"
  (flet ((brightness (n) (/ (apply #'+ (map 'list (lambda (x y) (* x y)) '(299 587 114) n))
                            1000)))
    (let* ((step (or step #x30))
           (bg (color-values (cdr (assoc 'background-color (frame-parameters)))))
           (background-brightness (brightness bg))
           (max-diff 125) ;; According to w3c
           (colors (loop summing step into n and until (>= n #x100) collect (list n)))
           (all (mapcan (lambda (n) (mapcar (lambda (m) (append n m)) colors)) colors))
           (all (mapcan (lambda (n) (mapcar (lambda (m) (append n m)) colors)) all))
           (all (remove-if
                 (lambda (n) (< (abs (- (brightness n) background-brightness)) max-diff))
                 all))
           (all (mapcar (lambda (n) (apply #'format "#%.2x%.2x%.2x" n)) all)))
      all)))

(defmacro with-focused-file-on-dired-mode-as-%file (&rest body)
  "To be used on dired-mode"
  `(let ((%file ,(dired-filename-at-point)))
     ,@body))


(provide 'gimme-utils)
;;; gimme-utils.el ends here
