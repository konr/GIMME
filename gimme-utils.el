;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(defun alpha-blend (c1 c2 a)
  "The resulting color of merging the c1 with alpha a on a background of color c2"
  (let* ((colors (mapcar (lambda (c) (list (substring c 1 3)
                                      (substring c 3 5)
                                      (substring c 5 7)))
                         (list c1 c2)))
         (colors (mapcar (lambda (c) (mapcar (lambda (e) (string-to-number e 16)) c))
                         colors))
         (color (map 'list (lambda (c1 c2) (format "%.2x" (+ (* (- 1 a) c1)
                                                        (* a c2))))
                     (nth 0 colors) (nth 1 colors))))
    (apply 'concat "#" color)))

(defun gimme-new-session () (setq gimme-session (random)))

(defmacro unlocking-buffer (&rest body)
  `(progn (toggle-read-only nil)
          ,@body
          (toggle-read-only t)))

(defun gimme-debug (&rest args)
  (let ((buffer-name (format "%s-debug" gimme-buffer-name)))
    (get-buffer-create buffer-name)
    (with-current-buffer buffer-name
      (goto-char (point-max))
      (mapcar #'insert args))))

(defun plist-to-alist (p)
  (loop for x = p then (cddr x) while x
        collecting (cons (car x) (cadr x))))

(defun plist-to-pseudo-alist (p)
  ;; FIXME: The sexp library won't work otherwise
  (loop for x = p then (cddr x) while x
        collecting (list (car x) (cadr x))))

(defun plist-subset (small big)
  "Returns non-nil if all key/vals in small are also in big"
  (let ((keys (loop for s = small then (cddr s) while s
                    collecting (car s))))
    (every (lambda (n) (equal (getf small n) (getf big n))) keys)))


(provide 'gimme-utils)
