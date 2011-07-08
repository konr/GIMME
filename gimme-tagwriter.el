(provide 'gimme-tagwriter)
(defvar gimme-tagwriter-max-length 20)

(defun gimme-tagwriter-mode ()
  "Displays a playlist"
  (interactive)
  (font-lock-mode)
  (use-local-map gimme-tagwriter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-tagwriter-mode
        mode-name "gimme-tagwriter"))

(defvar gimme-tagwriter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "?") 'gimme-print-current-field)
    (define-key map (kbd "TAB") 'gimme-tagwriter-next-field)
    (define-key map (kbd "<backtab>") 'gimme-tagwriter-prev-field)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Tagwriter's keymap")

(defun gimme-tagwriter ()
  (interactive)
  (let ((data (gimme-tagwriter-fix-data
               (apply #'range-to-plists (range-of-region)))))
    (gimme-on-buffer
     "gimme-tagwriter"
     (gimme-tagwriter-mode)
     (delete-region (point-min) (point-max))
     (let* ((colls (length (car data)))
            (rows (length data)))
       (make-local-variable 'colls) (set 'colls colls)
       (make-local-variable 'rows) (set 'rows rows)
       (gimme-tagwriter-create-table rows colls)
       (loop for row in data and i = 0 then (1+ i) doing
             (loop for coll in row and j = 0 then (1+ j)
                   doing (gimme-tagwriter-set-coll i j coll))))))
  (switch-to-buffer "gimme-tagwriter"))

(defun gimme-tagwriter-set-coll (row coll val)
  (save-excursion
    (let* ((bounds (gimme-tagwriter-cell-boundaries row coll))
           (beg (car bounds)) (end (cadr bounds)))
      (delete-region beg end) (goto-char beg)
      (insert (format " %s " val))
      (let ((largest (loop for r from 0 upto (1- rows) collecting
                           (progn (goto-char (cadr (gimme-tagwriter-cell-boundaries r coll)))
                                  (current-column))
                           into n and finally return (apply #'max n))))
        (loop for r from 0 upto (1- rows)
              doing (let ((end (cadr (gimme-tagwriter-cell-boundaries r coll))))
                      (goto-char end)
                      (dotimes (i (- largest (current-column))) (insert " "))))))))

(defun gimme-tagwriter-cell-boundaries (row coll)
  "Assuming the entire document is a table"
  (goto-line (1+ row))
  (beginning-of-line)
  (if (equal coll 0)
      (list (1+ (point)) (1- (car (gimme-tagwriter-cell-boundaries row 1))))
    (loop for i from 0 upto coll
          collecting (progn (forward-char) (find-char-forward ?|) (point)) into all
          finally return (list (+ 2 (car (last (butlast all)))) (+ 1 (car (last all)))))))

(defun find-char-forward (char)
  (unless (equal (char-after (1+ (point))) char)
    (forward-char)
    (find-char-forward char)))

(defun gimme-tagwriter-create-table (rows colls)
  (loop for i from 1 upto rows doing
        (insert
         (loop for j from 0 upto colls collecting "| " into bars
               finally return (format "%s\n" (apply #'concat bars))))))

(defun gimme-tagwriter-next-field ()
  (interactive)
  (let* ((cur (gimme-tagwriter-current-field))
         (next (list (car cur) (1+ (cadr cur))))
         (next (if (>= (cadr next) colls) (list (1+ (car cur)) 0) next))
         (next (if (>= (car next) rows) (list 1 0) next)))
    (message (format "%s" next))
    (goto-char (1+ (car (apply #'gimme-tagwriter-cell-boundaries next))))
    (gimme-print-current-field)))

(defun gimme-tagwriter-prev-field ()
  (interactive)
  (let* ((cur (gimme-tagwriter-current-field))
         (prev (list (car cur) (1- (cadr cur))))
         (prev (if (< (cadr prev) 0) (list (1- (car cur)) (1- colls)) prev))
         (prev (if (< (car prev) 1) (list (1- rows) (1- colls)) prev)))
    (goto-char (1+ (car (apply #'gimme-tagwriter-cell-boundaries prev))))
    (gimme-print-current-field)))

(defun gimme-tagwriter-current-field ()
  (list (max 0 (1- (line-number-at-pos)))
        (max 0 (1+ (- colls (length (split-string (substring (buffer-substring-no-properties (line-beginning-position) (line-end-position)) (current-column)) "|")))))))

(defun gimme-print-current-field ()
  (interactive)
  (let ((vals (get-text-property (point) 'vals)))
    (when vals (message (replace-regexp-in-string "%" "%%" (car vals))))))


(defun gimme-tagwriter-fix-data (data)
  (let* ((relevant (loop for j in data collecting
                         (loop for i = j then (cddr i) while i
                               if (not (member (car i) '(duration id font-lock-face pos)))
                               collect (list (car i) (cadr i)) into pairs end
                               finally return (mapcan (lambda (x) x) pairs))))
         (keys (loop for i = (car relevant) then (cddr i) while i collecting
                     (propertize (symbol-name (car i)) 'font-lock-face `(:foreground ,(color-for (symbol-name (car i))) :weight bold))))
         (vals (loop for j in relevant collecting
                     (loop for i = j then (cddr i) while i collecting
                           (propertize
                            (if (>= (length (cadr i)) gimme-tagwriter-max-length)
                                (format "%s..." (substring (cadr i) 0 (- gimme-tagwriter-max-length 3))) (cadr i))
                            'font-lock-face `(:foreground ,(color-for (symbol-name (car i))))
                            'type (car i) 'vals (list (cadr i)))))))
    (cons keys vals)))

(defun gimme-tagwrite-eval-formula (formula)
  (let*
      ((plist (range-to-plists (line-beginning-position) (line-end-position)))
       (plist (loop for item in plist if item
                    collect (list (plist-get item 'type) (car (plist-get item 'vals)))
                    into alist end finally return (mapcan (lambda (x) x) alist)))
       (text (replace-regexp-in-string
              "$[^ ]\+"
              (lambda (x) (format "%s" (plist-get plist (intern (substring x 1)))))
              formula)))
    text))

(defun decode-percent-encoding (string)
  (decode-coding-string (url-unhex-string string) 'utf-8))

(defun regexp-all-matches (string regexp)
  (with-temp-buffer
    (insert string) (goto-char (point-min))
    (loop while (ignore-errors (search-forward-regexp regexp)) collecting
	  (loop for i = 1 then (1+ i) while (match-string i)
		collecting (match-string i)))))

(defun build-all-matches (string regexp)
  (let* ((new-regexp (replace-regexp-in-string "$[a-zA-Z]\+" "\\\\\(\.\*\\\\\)" regexp)))))
