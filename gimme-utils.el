;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;


(defun gimme-new-session () 
  "The session is used to not mix data from two requests"
  (setq gimme-session (random)))

(defmacro unlocking-buffer (&rest body)
  `(progn (toggle-read-only nil)
          ,@body
          (toggle-read-only t)))

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
  (let* ((colors cool-colors)
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

(defun range-to-plists (p1 p2)
  "Returns all plists between points p1 and p2"
  (let ((min (min p1 p2)) (max (max p1 p2)))
    (loop for point = min then (next-property-change point)
          while (and point (> max point))
          collecting (text-properties-at point))))


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

(defun appropriate-colors ()
  "Returns all colors that, according to w3c, will be readable on your background"
  (flet ((brightness (n) (/ (apply #'+ (map 'list (lambda (x y) (* x y)) '(299 587 114) n))
                            1000)))
    (let* ((step #x30)
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

(defvar cool-colors (appropriate-colors))

(provide 'gimme-utils)
