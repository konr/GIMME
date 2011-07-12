(defvar gimme-tagwriter-max-length 33)

(defun gimme-tagwriter-mode ()
  "Displays a playlist"
  (interactive)
  (font-lock-mode)
  (use-local-map gimme-tagwriter-map)
  (setq truncate-lines t)
  (setq major-mode 'gimme-tagwriter-mode
        mode-name "gimme-tagwriter")
  (setq-local previous-function nil)
  (add-hook 'minibuffer-exit-hook #'gimme-tagwriter-undo-previews nil t))

(defvar gimme-tagwriter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "RET") 'gimme-tagwriter-apply-function)
    (define-key map (kbd "S-<return>") 'gimme-tagwriter-apply-previous-function)
    (define-key map (kbd "W") 'gimme-tagwriter-write-to-mlib)
    (define-key map (kbd "?") 'gimme-print-current-field)
    (define-key map (kbd "TAB") 'gimme-tagwriter-next-field)
    (define-key map (kbd "S") 'gimme-tagwriter-scan-current)
    (define-key map (kbd "<backtab>") 'gimme-tagwriter-prev-field)
    (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Tagwriter's keymap")

(defvar gimme-tagwriter-scan-map
  (let ((map minibuffer-local-map))
    (define-key map (kbd "TAB") 'gimme-tagwriter-recalculate-tags)
    map)
  "Tagwriter's scan keymap")

(defvar gimme-tagwriter-filter-map
  (let ((map minibuffer-local-map))
    (define-key map (kbd "TAB") 'gimme-tagwriter-recalculate-tags)
    map)
  "Tagwriter's filter keymap")


(defun gimme-tagwriter ()
  (interactive)
  (let* ((old-and-fixed (gimme-tagwriter-fix-data
                         (apply #'range-to-plists (range-of-region))))
         (fixed (cadr old-and-fixed)))
    (gimme-on-buffer
     "gimme-tagwriter"
     (gimme-tagwriter-mode)
     (delete-region (point-min) (point-max))
     (let* ((colls (length (car fixed)))
            (rows (length fixed)))
       (setq-local data (car old-and-fixed))
       (setq-local colls colls) (setq-local rows rows)
       (gimme-tagwriter-create-table rows colls)
       (loop for row in fixed and i = 0 then (1+ i) doing
             (loop for coll in row and j = 0 then (1+ j)
                   doing (gimme-tagwriter-set-cell i j coll))))))
  (switch-to-buffer "gimme-tagwriter"))

(defun gimme-tagwriter-set-cell (row coll val)
  (save-excursion
    (unlocking-buffer
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
                       (dotimes (i (- largest (current-column))) (insert " ")))))))))

(defun gimme-tagwriter-get-cell (row coll)
  (save-excursion
    (goto-char (1+ (car (gimme-tagwriter-cell-boundaries row coll))))
    (text-properties-at (point))))

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
        (insert (loop for j from 0 upto colls collecting "| " into bars
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
  (let ((vals (get-text-property (point) 'val)))
    (when vals (message (replace-regexp-in-string "%" "%%" vals)))))


(defun gimme-tagwriter-fix-data (data)
  (let* ((relevant (loop for j in data and line = 1 then (1+ line) collecting
                         (loop for i = j then (cddr i) while i
                               if (not (member (car i) '(duration id font-lock-face pos)))
                               collect (list (car i) (format "%s" (cadr i))) into pairs end
                               finally return (mapcan (lambda (x) x) pairs))))
         (keys (loop for i = (car relevant) then (cddr i) while i collecting
                     (propertize (symbol-name (car i)) 'font-lock-face `(:foreground ,(color-for (symbol-name (car i))) :weight bold))))
         (vals (loop for j in relevant collecting
                     (loop for i = j then (cddr i) while i collecting
                           (propertize
                            (if (>= (length (cadr i)) gimme-tagwriter-max-length)
                                (format "%s..." (substring (cadr i) 0 (- gimme-tagwriter-max-length 3))) (cadr i))
                            'font-lock-face `(:foreground ,(color-for (symbol-name (car i))))
                            'type (car i) 'val (cadr i))))))
    (list data (cons keys vals))))

(defun gimme-tagwriter-eval-formula (formula)
  (let*
      ((plist (range-to-plists (line-beginning-position) (line-end-position)))
       (plist (loop for item in plist if item
                    collect (list (plist-get item 'type) (plist-get item 'val))
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
    (loop for x = (ignore-errors (search-forward-regexp regexp))
          then (ignore-errors (search-forward-regexp regexp))
          and y = -1 then x while (and x (not (= 0 (- x y)))) collecting
          (loop for i = 1 then (1+ i) while (match-string i)
                collecting (match-string i)))))

(defun gimme-tagwriter-scan (string regexp)
  (let* ((new-regexp (replace-regexp-in-string "$[a-zA-Z]\+" "\\\\\(\.\*\\\\\)"
                                               regexp))
         (symbols (mapcar (lambda (x) (intern (substring (car x) 1)))
                          (regexp-all-matches regexp "\\($[a-zA-Z]\+\\)")))
         (matches (car (regexp-all-matches string new-regexp))))
    (loop for i = 0 then (1+ i) while (nth i matches)
          collecting (list (nth i symbols) (nth i matches)) into alist
          and finally return (mapcan (lambda (x) x) alist))))

(defun gimme-tagwriter-get-field (line-number field)
  (plist-get (gimme-tagwriter-get-vals line-number) field))

(defun gimme-tagwriter-get-vals (line-number)
  (let* ((range (save-excursion (goto-line line-number)
                                (list (line-beginning-position) (line-end-position))))
         (line (apply #'range-to-plists range))
         (plist (loop for f in line if f
                      collect (list (plist-get f 'type) (plist-get f 'val))
                      into alist and finally return (mapcan (lambda (x) x) alist))))
    plist))

(defun gimme-tagwriter-scan-current ()
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
     (let* ((regexp (read-from-minibuffer (format "%s> " url) ""
                                          gimme-tagwriter-scan-map))
            (plist (gimme-tagwriter-scan url regexp)))
       (gimme-tagwriter-set-vals line plist)))))

(defun gimme-tagwriter-undo-previews ()
  (with-current-buffer "gimme-tagwriter"
    (undo undos) (setq-local undos 0)))


(defun gimme-tagwriter-set-vals (line-number plist)
  (let* ((range (save-excursion (goto-line line-number)
                                (list (line-beginning-position) (line-end-position))))
         (tags (loop for f in (apply #'range-to-plists range)
                     if f collect (plist-get f 'type))))
    (loop for tag in tags and i = 0 then (1+ i) if (member tag plist)
          do (progn (gimme-tagwriter-update-cell line-number i (getf plist tag))
                    (setq-local undos (1+ undos))))))

(defun gimme-tagwriter-update-cell (line coll val)
  (let* ((old (gimme-tagwriter-get-cell (1- line) coll))
         (new-plist (plist-put old 'val val)) (max gimme-tagwriter-max-length))
    (gimme-tagwriter-set-cell
     (1- line) coll
     (apply #'propertize
            (if (>= (length val) max)
                (format "%s..." (substring val 0 (- max 3))) val) new-plist))))

(defun gimme-tagwriter-recalculate-tags ()
  (interactive)
  (let* ((regexp (minibuffer-contents))
         (url (with-current-buffer "gimme-tagwriter" url))
         (plist (gimme-tagwriter-scan url regexp)))
    (with-current-buffer "gimme-tagwriter"
      (unlocking-buffer
       (comment gimme-tagwriter-undo-previews)
       (gimme-tagwriter-set-vals line plist)))))


(defun gimme-tagwriter-write-to-mlib ()
  (interactive)
  (when (y-or-n-p "Write tags to the Medialib? ")
    (loop for datum in data and line = 1 then (1+ line) collecting
          (loop for head = (gimme-tagwriter-get-vals (1+ line)) then (cddr head)
                and new-plist = (plist-put datum (car head) (cadr head))
                then (plist-put datum (car head) (cadr head))
                while head finally return (butlast (butlast new-plist)))
          into plists and finally
          (dolist (plist plists)
            (gimme-send-message "(update_tags %s)\n"
                                (prin1-to-string (plist-to-pseudo-alist plist)))))))

(defun plist-merge (old new)
  (loop for head = new then (cddr head)
        and new-plist = (plist-put old (car head) (cadr head))
        then (plist-put old (car head) (cadr head))
        while head finally return (butlast (butlast new-plist))))

(defun gimme-tagwriter-apply-previous-function ()
  (interactive)
  (gimme-tagwriter-apply-function previous-function))

(defun gimme-tagwriter-apply-function (&optional raw)
  (interactive)
  (let* ((raw (or raw (read-from-minibuffer (format "Change to/with? " url) ""
                                            gimme-tagwriter-filter-map)))
         (function (read raw))
         (current-field (gimme-tagwriter-current-field))
         (data (plist-get (apply #'gimme-tagwriter-get-cell current-field) 'val))
         (current-field (list (1+ (car current-field)) (cadr current-field)))
         (processed (if (functionp function) (funcall function data) raw)))
    (apply #'gimme-tagwriter-update-cell `(,@current-field ,processed))
    (setq previous-function raw)))

(provide 'gimme-tagwriter)


