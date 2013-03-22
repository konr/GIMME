;;; ecoli.el --- Emacs Collection Library

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: CHI, Interface

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

;; TODO

;;; Code

(defvar ecoli-required '(ecoli-separator))
(defvar ecoli-n 10)

(defun ecoli-gen-basic-map ()
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q")   'kill-current-buffer)
    (define-key map (kbd "j")   'ecoli-next)
    (define-key map (kbd "k")   'ecoli-prev)
    (define-key map (kbd "C-f") 'ecoli-next-n)
    (define-key map (kbd "C-b") 'ecoli-prev-n)
    (define-key map (kbd "?")   'ecoli-show-keymap)
    (define-key map (kbd "h")   'ecoli-show-keymap)
    (define-key map (kbd "TAB")   'ecoli-toggle-view)
    map))

(defvar ecoli-basic-map (ecoli-gen-basic-map))

(defun plist-del (plist key)
  (loop for x = plist then (cddr x) while x
        unless (equal key (car x)) collect (list (car x) (cadr x))
        into alist and finally return (apply #'append alist)))

(defvar ecoli-basic-format-function
  (lambda (symbol) (propertize (format "%s" symbol) 'font-lock-face `(:foreground ,(color-for symbol)))))

(defvar ecoli-fancy-format-function
  (lambda (plist)
    (let ((plist (plist-del plist 'font-lock-face)))
      (eval `(let ((%plist ',plist)
                   ,@(loop for n = plist then (cddr n) while n
                           collecting (list (intern (format "%%%s" (car n)))
                                            (if (and (symbolp (cadr n)) (not (null (cadr n))))
                                                (list 'quote (cadr n)) (list 'quote (cadr n))))))
               (eval (car ecoli-formats)))))))

(defun ecoli-next ()
  "Goes to the next item."
  (interactive)
  (ecoli-next-n 0))

(defun ecoli-prev ()
  "Goes to the previous item."
  (interactive)
  (ecoli-prev-n 0))

(defun ecoli-next-n (&optional n)
  "Goes to the next n items."
  (interactive)
  (let* ((n (or n ecoli-n)) (p (point))
         (unsorted (remove-if (lambda (x) (<= x p)) ecoli-index))
         (sorted (sort unsorted #'<))
         (next (nth n sorted)))
    (if next (goto-char next) (goto-char (car ecoli-index)))))

(defun ecoli-prev-n (&optional n)
  "Goes to the previous n items."
  (interactive)
  (let* ((n (or n ecoli-n)) (p (point))
         (unsorted (remove-if (lambda (x) (>= x p)) ecoli-index))
         (sorted (sort unsorted #'<))
         (next (nth (- (length sorted) n 1) sorted)))
    (if next (goto-char next) (goto-char (car (last ecoli-index))))))

(defun ecoli-show-keymap ()
  "Shows the available keybindings"
  (interactive)
  (ecoli-on-buffer
   name
   (let* ((name "Ecoli - Keybindings")
          (map (current-local-map))
          (header "Here are the currently set keybindings.\n\n---\n\n"))
     (delete-region (point-min) (point-max))
     (insert header)
     (map-keymap
      (lambda (event function)
        (when (and (atom event) (functionp function))
          (let ((key (if (numberp event)
                         (key-description (format "%c" event))
                       (format "%s" event)))
                (nodocs (propertize "FIXME: NO DOCS FOUND!" 'font-lock-face '(:foreground "#ff0000")))
                (docs (documentation function)))
            (setq key (propertize key 'font-lock-face '(:weight bold)))
            (setq docs (car (split-string (or docs nodocs) "\n")))
            (insert (format "%s%s%s\n" key
                            (make-string (ceiling (/ (- (* 2.0 tab-width) (length key)) tab-width)) ?\t) docs)))))
      map)
     (font-lock-mode 1)
     (use-local-map ecoli-basic-map)
     (switch-to-buffer name))))

(defun ecoli-separator (&optional sep)
  (let ((sep (or sep ecoli-separator)))
    (propertize (case (type-of sep) (string sep) (symbol (funcall sep)) (t "Error!"))
                'ecoli-type 'separator)))


(defmacro ecoli-on-buffer (name &rest body)
  "Macro that does all the nasty things required to modify a buffer"
  `(with-current-buffer (get-buffer-create ,name)
     (unlocking-buffer (save-excursion (goto-char (point-max)) ,@body))))

(defun ecoli-gen-buffer (app-plist setup-plist function)
  "Generates a Buffer and sets the variables correctly"
  (let* ((buffer-name (plist-get app-plist 'buffer-name)))
    (ecoli-on-buffer buffer-name
                     (delete-region (point-min) (point-max))
                     ;;(kill-all-local-variables)
                     (use-local-map (if (member 'ecoli-map setup-plist) (plist-get setup-plist 'ecoli-map) ecoli-basic-map))
                     (setq-local ecoli-index nil)
                     (setq-local ecoli-min nil)
                     (setq-local ecoli-format-function ecoli-basic-format-function)
                     (setq-local ecoli-separator "\n")
                     (loop for x = setup-plist then (cddr x) while x doing
                           (let ((key (car x)) (val (cadr x))) (make-local-variable key) (set key val)))
                     (loop for x = app-plist then (cddr x) while x doing
                           (let ((key (car x)) (val (cadr x))) (make-local-variable key) (set key val)))
                     (font-lock-mode 1)
                     (funcall function)))
  (switch-to-buffer (get-buffer buffer-name))
  buffer-name)

(defun ecoli-append (buffer item)
  (ecoli-on-buffer
   buffer
   (let ((sep (ecoli-separator))
         (formatted (funcall ecoli-format-function item))
         (max (point-max)))
     (setq-local ecoli-index (append ecoli-index (list max)))
     (goto-char max)
     (insert (format "%s%s" formatted sep)))))

(defun ecoli-insert (buffer item position)
  (ecoli-on-buffer
   buffer
   (let* ((sep (ecoli-separator))
          (formatted (funcall ecoli-format-function item))
          (sorted (append (sort ecoli-index #'<) (list (point-max))))
          (locus (nth position sorted)))
     (setq-local ecoli-index (cons locus ecoli-index))
     (goto-char locus)
     (insert (format "%s%s" formatted sep))
     (ecoli-reindex))))


(defun ecoli-between-region (&optional indexes)
  "Returns the ____"
  (let* ((both (if (use-region-p)
                   (list (point) (mark))
                 (list (point) (point))))
         (index (append ecoli-index (list (point-max))))
         (min (apply #'min both)) (max (apply #'max both))
         (min (apply #'max (remove-if (lambda (x) (> x min)) index)))
         (max (apply #'min (remove-if (lambda (x) (<= x max)) index))))
    (if (not indexes) (list min max)
      (loop for x in index and i = 0 then (1+ i)
            when (or (= x min) (= x max)) collect i))))


(defun ecoli-index-to-regions (&optional index sep)
  (let* ((index (sort (or index ecoli-index) #'<))
         (sep (or sep (ecoli-separator)))
         (len (length sep))
         (max (point-max)))
    (map 'list (lambda (x y) (cons x (- y len))) index (append (cdr index) (list (point-max))))))

(defun ecoli-reindex ()
  (save-excursion
    (let ((min (or ecoli-min (point-min))))
      (goto-char min)
      (loop while (text-property-any (point) (point-max) 'ecoli-type 'separator)
            doing (goto-char (1+ (or (text-property-any (point) (point-max) 'ecoli-type 'separator) (point-min))))
            collecting (point) into points
            finally do (setq-local ecoli-index (cons min (butlast points)))))))

(defun ecoli-toggle-view ()
  "Cycle through the views."
  (interactive)
  (setq-local ecoli-formats (append (cdr ecoli-formats) (list (car ecoli-formats))))
  (save-excursion
    (unlocking-buffer
     (funcall ecoli-before-update-function)
     (loop for pair in (reverse (ecoli-index-to-regions)) collecting
           (let* ((beg (car pair)) (end (cdr pair))
                  (prop (text-properties-at beg))
                  (formatted (funcall ecoli-format-function prop)))
             (delete-region beg end)
             (goto-char beg)
             (insert formatted)))
     (ecoli-reindex))))

(defun ecoli-delete-item (pos)
  (ecoli-on-buffer
   (current-buffer)
   (delete-region (nth pos ecoli-index) (or (nth (1+ pos) ecoli-index) (point-max)))
   (ecoli-reindex)))

(defun ecoli-update-item (new)
  (let ((pos (getf new 'pos)))
    (ecoli-on-buffer (current-buffer)
                     (delete-region (nth pos ecoli-index) (or (nth (1+ pos) ecoli-index) (point-max)))
                     (ecoli-insert (current-buffer) new pos))))

;;;;;;;;;;;
;; utils ;;
;;;;;;;;;;;

(defun ecoli-range-to-plists (&optional p1 p2)
  "Returns all plists between points p1 and p2"
  (let ((min (min p1 p2)) (max (max p1 p2)))
    (loop for point = min then (next-property-change point)
          while (and point (> max point))
          collecting (text-properties-at point))))

(defun ecoli-plists-on-region ()
  (interactive)
  (remove-if (lambda (x) (equal x '(ecoli-type separator)))
             (apply #'ecoli-range-to-plists (ecoli-between-region))))

(defun plist-keys (plist)
  (loop for x = plist then (cddr x) while x
        collecting (car x)))

(defun plist-values (plist)
  (loop for x = plist then (cddr x) while x
        collecting (cadr x)))

(defmacro ->> (x &optional form &rest more)
  "Clojure's macro"
  (if (null form) x
    (if (null more)
        (if (sequencep form)
            `(,(car form) ,@(cdr form) ,x)
          (list form x))
      `(->> (->> ,x ,form) ,@more))))

(defmacro -> (x &optional form &rest more)
  "Clojure's macro"
  (if (null form) x
    (if (null more)
        (if (sequencep form)
            `(,(car form) ,x ,@(cdr form))
          (list form x))
      `(-> (-> ,x ,form) ,@more))))

(defun transpose (list-of-lists)
  "((a b c) (d e f)) -> ((a d) (b e) (c f)). Amazing, huh?"
  (apply #'map 'list (lambda (&rest args) (apply #'list args)) list-of-lists))

(defun mapi1000 (function &rest lists)
  (apply #'map 'list function (loop for i upto 1000 collecting i) lists))

(defun plist-add (minor major)
  (loop for x = minor then (cddr x) 
	and y = major then (plist-put y (car x) (cadr x))
	while x finally return y))

(provide 'ecoli)
